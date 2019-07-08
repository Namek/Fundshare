create table Users
	( id int primary key autoincrement                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
	, email string(255) unique
	, name string(255)
	, password_hash string(255)
	, inserted_at datetime
	, updated_at datetime
	)

create table Tags
	( id int primary key autoincrement
	, tag string (100)
	)

create table Transactions_Tags
	( transaction_id int references Transactions(id)
	, tag_id int references Tags(id)
	)

create table Transactions
	( id int primary key autoincrement
	, author_id int references Users(id)
	, payor_id int references Users(id)
	, beneficient_ids -- TODO many
	, amount int
	, description string(1024)
	, tags_ids int --references Transactions_Tags(transaction_id)
	, inserted_at datetime
	, updated_at datetime
	, acceptance_ids -- TODO many
	)

-- this table is like a cache calculated based on all Transactions
create table Balances
	( user1_id int references Users(id)
	, user2_id int references Users(id)
	, balance_num int
	, balance_den int
	, user1_has_more bool
	, shared_payment_count int
	, transfer_count int
	, last_update_at datetime
	, inserted_at datetime
	, updated_at datetime
	, authored_by_user1_count int
	, authored_by_user2_count int
	, inbox_for_user1_count int
	, inbox_for_user2_count int
	)


--create index IX_Comments_AuthorId on Comments
--    (AuthorId)