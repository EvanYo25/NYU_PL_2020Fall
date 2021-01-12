-- main
with Text_IO, Ada.Integer_Text_IO, msort;
use Text_IO, Ada.Integer_Text_IO, msort;

procedure Main is
	-- local variables and functions
	mysum: Integer:= 0;

	Task Printer is
		entry startPrinter;
	end Printer;
	Task body Printer is
	begin
		accept startPrinter;
		for i in 1..msort.LENGTH
		loop
			Put(Int_Range'Image(msort.myArr(i))); Put(" ");
		end loop;
		New_Line;
		Put(Integer'Image(mysum)); New_Line;
	end Printer;


	Task Sum is 
		entry startSum;
	end Sum;
	Task body Sum is
	begin
		accept startSum;
		mysum:= 0;
		for i in 1..msort.LENGTH
		loop
			mysum:= mysum+ Integer(msort.myArr(i));
		end loop;
	end Sum;


	Task Reader is
		entry startReader;
		entry doneReader;
	end Reader;
	Task body Reader is
		X: Integer := 0;
	begin
		accept startReader;
		for i in 1 .. msort.LENGTH
		loop
			Get(X);
			msort.myArr(i):= Int_Range(X);
			--Put(i); Put(X); New_Line;
		end loop;
		Sum.startSum;
		accept doneReader;
	end Reader;

begin
	Reader.startReader;
	Reader.doneReader;
	msort.sort(msort.myArr);
	Printer.startPrinter;

end Main;

