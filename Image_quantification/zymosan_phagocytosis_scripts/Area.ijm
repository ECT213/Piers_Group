//select images directory
input = getDirectory("input folder");
//get list of files in folder
list = getFileList(input)
//using combined images
setBatchMode("hide");

//red area = red -green in calc plus
run("Split Channels");
run("Calculator Plus", "title=[red image] title=[green image] operation=[Scale: i2 = i1 x k1 + k2] k1=1 k2=0 create");
selectImage("Result");
//run("Threshold...");
setThreshold(190, 255);
run("Analyze Particles...", "summarize");

//cell area = green-blue in calc plus
run("Split Channels");
run("Calculator Plus", "title=[blue image] title=[green image] operation=[Scale: i2 = i1 x k1 + k2] k1=1 k2=0 create");
	selectImage("Result");
	//run("Threshold...");
	setThreshold(0, 150, "raw");
run("Convert to Mask");
run("Fill Holes");
run("Analyze Particles...", "summarize");


for (i=0; i<list.length; i++) {
	open(input+list[i]);
	
run("Split Channels");
	run("Calculator Plus", "title=[red image] title=[green image] operation=[Scale: i2 = i1 x k1 + k2] k1=1 k2=0 create");
	selectImage("Result");
	setAutoThreshold("Default no-reset");
	//run("Threshold...");
	setThreshold(0, 155, "raw");
	run("Analyze Particles...", "summarize");
	
}

run("Calculator Plus", "title=[red image] title=[green image] operation=[Scale: i2 = i1 x k1 + k2] k1=1 k2=0 create");
setAutoThreshold("Default no-reset");
//run("Threshold...");
//setThreshold(190, 255);
setOption("BlackBackground", true);
run("Convert to Mask");
run("Analyze Particles...", "summarize");

//using light images
//setThreshold(185, 255);
setOption("BlackBackground", true);
run("Convert to Mask");
run("Analyze Particles...", "clear summarize");
