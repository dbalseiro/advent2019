drop table intersection;

create table intersection as
select a.x, a.y 
from point a, point b
where a.x = b.x and a.y = b.y
  and a.path_id = 1 and b.path_id = 2;
 