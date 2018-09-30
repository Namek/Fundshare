#! "netcoreapp2.1"

using System.Runtime.CompilerServices;
using System.Threading;

public static string GetScriptFolder([CallerFilePath] string path = null) =>
    Path.GetDirectoryName(path);

var currentDir = GetScriptFolder();
var outputDir = Path.Combine(currentDir, "dist");
var srcDir = Path.Combine(currentDir, "src");
var staticsFolder = Path.Combine(srcDir, "static");

if (Args.Count == 0)
{
    log("Usage: build / watch");
    return 0;
}


if (!Directory.Exists(srcDir))
{
    Console.Error.WriteLine("Directory './src' does not exist!");
    Console.Error.Flush();
    return 1;
}

if (!Directory.Exists(outputDir))
{
    string odir = Path.GetFullPath(outputDir);
    log($"Creating directory: {odir}");
    Directory.CreateDirectory(odir);
}

if (Args.Contains("build"))
{
    copyStatics();
    buildElm();
    buildScss();
}


var lastUpdates = new Dictionary<string, long>();

if (Args.Contains("watch"))
{
    var watcher = new FileSystemWatcher(srcDir)
    {
        EnableRaisingEvents = true,
        IncludeSubdirectories = true
    };

    watcher.Created += (_, args) => onFileChanged(WatcherChangeTypes.Created, args);
    watcher.Deleted += (_, args) => onFileChanged(WatcherChangeTypes.Deleted, args);
    watcher.Renamed += (_, args) => onFileChanged(WatcherChangeTypes.Renamed, args);
    watcher.Changed += (_, args) => onFileChanged(WatcherChangeTypes.Changed, args);

    log("\nWatching for changes...\n");
    Thread.CurrentThread.Join();

}

void onFileChanged(WatcherChangeTypes evt, FileSystemEventArgs args)
{
    bool shouldInvoke = false;

    lock (lastUpdates)
    {
        if (!lastUpdates.TryGetValue(args.FullPath, out var lastUpdate))
            shouldInvoke = true;
        else
            shouldInvoke = (DateTime.Now.ToFileTimeUtc() - lastUpdate) > 50/*ms*/ * 10000;

        //log((DateTime.Now.ToFileTimeUtc() - lastUpdate).ToString());

        if (shouldInvoke) {
            Task.Run(async () => {
                await Task.Delay(300);
                _onFileChanged(evt, args);
            });

            lastUpdates.Remove(args.FullPath);
            lastUpdates.Add(args.FullPath, DateTime.Now.ToFileTimeUtc());
        }
    }
}
void _onFileChanged(WatcherChangeTypes evt, FileSystemEventArgs args)
{
    var path = args.FullPath.Replace('\\', '/');
    var srcSubfolder = Path.GetDirectoryName(path)
        .Replace('\\', '/').Split('/').Last();

    log($"Changed: {path}");

    bool notifyBrowser = true;

    if (path.Contains("src/elm") && (path.EndsWith(".elm") || path.EndsWith(".js")))
        buildElm();
    else if (path.EndsWith(".scss"))
        buildScss();
    else if (path.Contains("src/static"))
        copyStatic(path);
    else
        notifyBrowser = false;

    if (notifyBrowser)
    {
        // TODO
    }
}


void buildElm()
{
    log("Building Elm code...");
    var input = Path.Combine(srcDir, "elm", "src", "Main.elm");
    var output = Path.Combine(outputDir, "js", "elm.js");
    var workDir = Path.Combine(currentDir, "src", "elm");
    exec(workDir, "elm", $@"make {input} --output={output} --debug");
    touchFile(output);
}

void buildScss()
{
    log("Building Sass code...");
    // TODO
}

void copyStatics()
{
    var files = Directory.EnumerateFiles(staticsFolder, "*", SearchOption.AllDirectories);

    log("Copying statics...");
    foreach (var file in files)
    {
        copyStatic(file, "  - ");
    }
}

void copyStatic(string filePath, string logPrefix = "Copying ")
{
    var dir = Path.GetDirectoryName(filePath);
    var shortPath = filePath.Replace(staticsFolder, "").TrimStart('\\').TrimStart('/');
    var finalPath = Path.Combine(outputDir, shortPath);
    var finalPathDir = Path.GetDirectoryName(finalPath);

    log($"{logPrefix}{shortPath}");

    if (!Directory.Exists(finalPathDir))
        Directory.CreateDirectory(finalPathDir);

    File.Copy(filePath, finalPath, true);
    //touchFile(finalPath);
}


void exec(String workDir, String cmd, String args = "")
{
    var procInfo = new ProcessStartInfo()
    {
        FileName = cmd,
        Arguments = args,
        WorkingDirectory = workDir
        //CreateNoWindow = true,
        //UseShellExecute = false
    };

    var proc = Process.Start(procInfo);
    proc.WaitForExit();
}

void touchFile(string path)
{
    var now = DateTime.Now;
    File.SetCreationTime(path, now);
    File.SetLastWriteTime(path, now);
    File.SetLastAccessTime(path, now);
}

void log(string text)
{
    Console.WriteLine(text);
}