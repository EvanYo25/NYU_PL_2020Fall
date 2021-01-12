-- msort body
with Text_Io, Ada.Integer_Text_IO;
use Text_Io, Ada.Integer_Text_IO;

package body msort is
	procedure Sort (m: in out Arr) is
		procedure Merge(left: in Integer; right: in Integer) is
			merged: Arr;
			mid: Integer:= (left+right)/2;
			i: Integer:= left;
			j: Integer:= mid+1;
			k: Integer:= left;
		begin
			-- Put_Line(Integer'Image(left) & " " & Integer'Image(right));
			-- Put_Line(Integer'Image(i) & " " & Integer'Image(j));
			while i<=mid or j<=right loop
				if i>mid then
					merged(k):= m(j);
					j:= j+1;
				elsif j>right then
					merged(k):= m(i);
					i:= i+1;				
				elsif m(i)<=m(j) then
					merged(k):= m(i);
					i:= i+1;
				else
					merged(k):= m(j);
					j:= j+1;
				end if;
				k:= k+1;
			end loop;
			for x in left..right loop
				-- Put(Int_Range'Image(merged(x))); Put(" ");
				m(x):= merged(x);
			end loop;
			-- New_Line;
		end;

		procedure MergeSort(left: in Integer; right: in Integer) is
			procedure doTasks is
				mid: Integer:= (left+right)/2;
				Task doleft;
				Task doright;

				Task body doleft is
				begin
					if left<mid then
						MergeSort(left,mid);
					end if;
				end doleft;

				Task body doright is
				begin
					if mid+1<right then
						MergeSort(mid+1,right);
					end if;
				end doright;
			begin
				null;
				-- Put_Line(Integer'Image(left) & Integer'Image(right));
			end;
		begin
			doTasks;
			Merge(left,right);
		end;
	begin
		MergeSort(1,LENGTH);
	end;
end msort;


