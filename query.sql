create database billing;

use billing;

create table admins(
username varchar(50),
password varchar(50),
primary key(username)
);

create table stock(
item_id int(5) NOT NULL auto_increment,
item_name varchar(50) unique not null,
item_price int(5) default 0,
item_stock int(10) default 0,
primary key(item_id)
);

create table transactions(
id int(5) NOT NULL auto_increment,
cust_name varchar(50),
payment varchar(30),
d_t date,
primary key(id)
);

create table total_transaction(
id int(5) NOT NULL auto_increment,
item_name varchar(50),
quantity int(10),
total int(10),
t_id varchar(50),
primary key(id)
);

show tables;

desc admins;
desc stock;
desc transactions;
desc total_transaction;



alter table total_transaction
add constraint t_id
foreign key (t_id)
references transactions(id)
on delete cascade;