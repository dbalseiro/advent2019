drop table point;

create table point (
	id serial primary key,
	path_id int references path(id),
	x int,
	y int,
	position int
);
