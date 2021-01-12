-- msort spec

package msort is
	LENGTH: Integer := 40;
	
	type Int_Range is range -300..300;
	type Arr is array(1..LENGTH) of Int_Range;
	myArr : Arr;

	procedure Sort(m: in out Arr);
end msort;
