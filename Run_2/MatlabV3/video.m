framesPath = 'D:\LiWenPeng\desk\pic\case3\cloud\';%图像序列所在路径，同时要保证图像大小相同
videoName = 'D:\LiWenPeng\desk\pic\cloudcase3v.avi';%表示将要创建的视频文件的名字
fps = 10; %帧率
startFrame = 1; %从哪一帧开始
endFrame = 60; %哪一帧结束
 
if(exist('videoName','file'))
    delete videoName.avi
end
 
%生成视频的参数设定
aviobj=VideoWriter(videoName);  %创建一个avi视频文件对象，开始时其为空
aviobj.FrameRate=fps;

open(aviobj);%Open file for writing video data
%读入图片
for i=startFrame:endFrame
    fileName=sprintf(['case3time',num2str(i) 'v']);    %根据文件名而定 
    frames=imread([framesPath,fileName,'.png']);
    writeVideo(aviobj,frames);
end
close(aviobj);% 关闭创建视频
