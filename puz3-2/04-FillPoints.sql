/* Move along the path and create the points */

-- select * from path_spec ps;

create or replace function fill_points() returns int as $function$
declare
	x int;
	y int;
	l int;

	rec_path_spec path_spec%rowtype;
	cur_path_spec cursor (p_path_id int)
		for select * from path_spec ps
		where ps.path_id = p_path_id;
begin
	delete from point;
	for i in 1..2 loop
		x := 0;
		y := 0;
		l := 0;
	
		open cur_path_spec(i);
		loop
			fetch cur_path_spec into rec_path_spec;
			exit when not found;
		
			for j in 1..rec_path_spec.steps loop
				case rec_path_spec.direction
					when 'R' then x := x + 1;
					when 'L' then x := x - 1;
					when 'U' then y := y + 1;
					when 'D' then y := y - 1;
					else return -1;
				end case;
				
				l := l + 1;
				
				insert into point (path_id, x, y, position) 
				values (i, x, y, l);
			end loop;
   		end loop;
   		close cur_path_spec;
 	end loop;

	return 0;
end; $function$

language plpgsql;

select fill_points();