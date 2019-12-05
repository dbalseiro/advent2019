drop table path;

create table path (
	id int primary key,
	description varchar(10) not null
);

insert into "path" values (1, 'PATH 1');
insert into "path" values (2, 'PATH 2');

drop table path_spec;

create table path_spec (
	id serial primary key,
	path_id int references path(id),
	direction char,
	steps int
);