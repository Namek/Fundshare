#! "netcoreapp2.1"

using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using System.Threading;

public static string GetScriptFolder([CallerFilePath] string path = null) =>
    Path.GetDirectoryName(path);

var currentDir = GetScriptFolder();
var outputDir = Path.Combine(currentDir, "public");
var srcDir = Path.Combine(currentDir, "src");
var staticsFolder = Path.Combine(srcDir, "static");
var stylesFolder = Path.Combine(srcDir, "css");
var generateDebugElm = false;

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

if (Args.Contains("debug"))
{
    generateDebugElm = true;
}

if (Args.Contains("api"))
{
    generateElmApi();
}

if (Args.Contains("build"))
{
    copyStatics();
    buildElm();
    buildStyles();
}

if (Args.Contains("build-elm"))
{
    buildElm();
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

    bool notify = true;

    if (path.Contains("src/elm") && (path.EndsWith(".elm") || path.EndsWith(".js")))
        buildElm();
    else if (path.EndsWith(".css"))
        buildStyles();
    else if (path.Contains("src/static"))
        copyStatic(path);
    else
        notify = false;

    if (notify)
    {
        log($"Changed: {path}");

        // TODO notify browser
    }
}


void buildElm()
{
    var msg = "Building Elm code...";
    var input = Path.Combine(srcDir, "elm", "Main.elm");
    var output = Path.Combine(outputDir, "js", "elm.js");
    var workDir = Path.Combine(currentDir, "src", "elm");
    var args = $@"make {input} --output={output}";

    if (generateDebugElm)
    {
        msg += " with debug!!";
        args += " --debug";
    }

    log(msg);
    exec(workDir, "elm", args);
    
    if (File.Exists(output))
        touchFile(output);
}

void generateElmApi()
{
	// This creates the src\elm\Api folder
    var workDir = Path.Combine(currentDir, "src", "elm");
    exec(workDir, @"C:\Program Files\nodejs\npx.cmd", "@dillonkearns/elm-graphql --introspection-file ../../../graphql_schema.json --output .");
}

void buildStyles()
{
    var files = Directory.EnumerateFiles(stylesFolder, "*.css", SearchOption.AllDirectories);
    var finalDir = Path.Combine(outputDir, "css");

    log("Copying styles...");
    foreach (var file in files)
    {
        var finalPath = Path.Combine(finalDir, Path.GetFileName(file));

        File.Copy(file, finalPath, true);
    }
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
        WorkingDirectory = workDir,
        RedirectStandardError = true,
        RedirectStandardOutput = true
    };

    var proc = Process.Start(procInfo);
    var err = proc.StandardError.ReadToEnd();
    var output = proc.StandardOutput.ReadToEnd();
    proc.WaitForExit();
    Console.Write(err);
    Console.Write(output);

    // this is specific to Elm compiler
    var matches = Regex.Matches(err, "-- CORRUPT BINARY - ([^\n]+)");

    bool shouldRetry = false;
    foreach (Match match in matches)
    {
        if (match.Groups.Count > 1)
        {
            var path = match.Groups[1].Value.Trim();
            var directory = Path.GetDirectoryName(path);

            if (Directory.Exists(directory))
            {

                var oldColor = Console.BackgroundColor;
                Console.BackgroundColor = ConsoleColor.Red;
                Console.WriteLine($"Deleting {directory}");
                Console.BackgroundColor = oldColor;

                Directory.Delete(directory, true);
                shouldRetry = true;
            }
        }
    }

    if (shouldRetry)
    {
        exec(workDir, cmd, args);
    }
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