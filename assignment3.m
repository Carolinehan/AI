I=imread('coins.tif');

f=[1 1 1;1 1 1;1 1 1]./9;
I=imfilter(I,f,'symmetric');
subplot(2, 3, 1);
imshow(I);
level = graythresh(I);
BW=im2bw(I,level);
subplot(2, 3, 2);
imshow(BW);
C=~BW;
subplot(2, 3, 3);
imshow(C);
% D=bwdist(C);
% D(C)=-Inf;
% L=watershed(D);
% L=C;
labeledImage = bwlabel(C, 8); 
subplot(2, 3, 4);
imshow(labeledImage, []);
stats=regionprops(labeledImage,'area');

idx = find([stats.Area] > 400)
BW2 = ismember(labeledImage, idx);  
subplot(2, 3, 4);
imshow(BW2);


