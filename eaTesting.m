(* ::Package:: *)

BeginPackage["eaTesting`"]

testPartialMatrices;

Begin["`Private`"]

Needs["eaDataStructure`",FileNameJoin[{NotebookDirectory[],"eaDataStructure.m"}]];


(*ChANGED*)
testPartialMatrices[graphData0_,vtList_]:=Module[{ds1,ds2,mat1,mat2,index,mats,faceCounts,rS1,rS2,dsR1,dsR2,cases1,cases2,pass=True,i,gr3index2,gr3index,j,k,ind,row,col,joined,l,seen,end,from,to,count,countFrom,countTo,p,cycleBooleans,graphData},
{index,mats}=getMatrixInfo[graphData0];
(*summing the matrix with its transpose first, this means that we get all pairs, and..shouldn't need to check columns*)
{ds1,ds2}=getDegreeSequence[graphData0];
faceCounts=getFaceCounts[graphData0];
rS1=faceCounts[[1]][[3]];
rS2=faceCounts[[2]][[3]];
dsR1=ds1-rS1;
dsR2=ds2-rS2;

If[pass,
If[!(hHTest[dsR1]&&hHTest[dsR2]),
pass=False;
];
];


(*indexes of vertices greater than 3, followed by their vertex degree and their vertex number*)
cycleBooleans=getCycleBooleans[graphData0];
i=1;
gr3index={};
While[i<=Length[ds2]&&ds2[[i]]>3,
If[!cycleBooleans[[i]]&&rS2[[i]]>=3&&MemberQ[vtList,i+Length[ds1]],
AppendTo[gr3index,{2,i,ds2[[i]],i+Length[ds1]}];
];
i++;
];

For[i=1,i<=Length[gr3index2],i++,
If[rS2[[i]]>=3,
AppendTo[gr3index,gr3index2[[i]]];
];
];

i=1;
While[i<=Length[gr3index]&&pass,
ind=gr3index[[i]];
row=mats[[ind[[1]]]][[ind[[2]]]];
col=Transpose[mats[[ind[[1]]]]][[ind[[2]]]];
(*this is simply to count how many of each vertex number are in the matrix*)
joined=Join[row,col];

j=1;
(*find a non zero pair of entries*)
l=Length[row];
(*seen is a list of entries that have already been considered*)
seen={};
While[j<=l&&pass,
While[j<=l&&(row[[j]]==0||col[[j]]==0||MemberQ[seen,j]&&pass),
j=j+1;
];
If[j<=l,
(*this means j represents an entry in the row and column which are non zero, therefore a pair to be considered*)

AppendTo[seen,j];
end=col[[j]];
from=end;
to=row[[j]];
count=1;
countFrom=Count[joined,from];
countTo=Count[joined,to];
k=j;
If[countFrom==2,
While[to!=end&&countTo==2&&count < l,
count=count+1;
p=Flatten[Position[joined,to]];
p=Table[Mod[pi-1,l]+1,{pi,p}];
p=DeleteCases[p,k];
k=p[[1]];
AppendTo[seen,k];
from=to;
If[row[[k]]==to,
to=col[[k]],
to=row[[k]];
];
countTo=Count[joined,to];
];
If[to==end&&count<gr3index[[i]][[3]],
pass=False;
];
If[to==end&&count==gr3index[[i]][[3]],
cycleBooleans[[gr3index[[i]][[2]]]]=True;
];
];
];
j=j+1;
];
i=i+1;
];
graphData=setCycleBooleans[graphData0,cycleBooleans];
Return[{pass,graphData}];
];

hHTest[degreesList0_]:=Module[{degreeList=degreesList0,degrees,freq,ret,failed,ds,di,fi,i,takesAndAdds,takeAdd,last},
(*can take a list which includes zeros*)
degreeList=DeleteCases[degreeList,0];
If[Length[degreeList]>0,

degreeList=Sort[degreeList,Greater];
degrees=Table[i,{i,Max[degreeList],1,-1}];
freq=Table[Count[degreeList,degi],{degi,degrees}];
ret=Table[{degrees[[i]],freq[[i]]},{i,1,Length[degrees]}];
failed=False;
While[!failed&&Length[ret]>0,

(*first get vertex to test*)
di=ret[[1]][[1]];
fi=ret[[1]][[2]];
(*Check if its the last one*)
If[fi==1,
ret=Delete[ret,1],
ret[[1]][[2]]=ret[[1]][[2]]-1;
];
i=1;
(*What to take and add from a listing*)
takesAndAdds=Table[0,{i,1,Length[ret]}];
(*termination is ok if di\[Equal]0 otherwise failed*)

While[di>0&&i<=Length[ret],
If[di<=ret[[i]][[2]],
takeAdd = di;
di=0,
takeAdd=ret[[i]][[2]];
di=di-ret[[i]][[2]];
];
takesAndAdds[[i]]=takeAdd;
i=i+1;
];
If[di>0,
failed=True,
For[i=1,i<Length[takesAndAdds],i++,
ret[[i]][[2]]=ret[[i]][[2]]-takesAndAdds[[i]];
ret[[i+1]][[2]]=ret[[i+1]][[2]]+takesAndAdds[[i]];
];
(*only take don't add*)
last=Length[takesAndAdds];
ret[[last]][[2]]=ret[[last]][[2]]-takesAndAdds[[last]];
];
(*first get vertex to test*)
di=ret[[1]][[1]];
fi=ret[[1]][[2]];
(*Check if its the last one*)

While[Length[ret]>0&&ret[[1]][[2]]==0,
ret=Delete[ret,1];
];


];
Return[!failed],
Return[True];
];
];
End[]
EndPackage[]
