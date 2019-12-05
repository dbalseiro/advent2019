drop table result;

create table result as
select p1.x, p1.y, (p1.position + p2.position) as len
from point p1, point p2
where (p1.x, p1.y) in (select * from intersection)
  and (p2.x, p2.y) in (select * from intersection)  
  and p1.x = p2.x and p1.y = p2.y
  and p1.path_id = 1 and p2.path_id = 2;

 select min(len) from result;