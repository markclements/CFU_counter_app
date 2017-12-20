library(purrr)
library(tidyverse)
library(magick)
library(stringr)
library(googledrive)

save_image <- function(img, img_name, output_dir) {
	image_write(img, file.path(output_dir, basename(img_name)))
	img
}

## convert all files to smaller file size (500)
list.files(path="Desktop/app-CFU_counter/images/",
					 full.names = T) %>%
	map(~{
		cur_fil<-.x
			image_read(cur_fil) %>%
			image_resize("500") %>%
			save_image(cur_fil,"Desktop/app-CFU_counter/images/")	
	})			 	

	
	
	
as_id("https://drive.google.com/open?id=0B-moOb822JLXWk1zV2Y1Z3VIUUU") %>%
	drive_ls()->directory
	
##View(x[1,]$drive_resource)

tibble(name=directory$name,
			 link=map_chr(directory$drive_resource,"webContentLink")) %>%
				arrange(name) %>%
	write_csv("files.csv")

x<-read_csv("files.csv")

select=x$name[1]

x %>%
	filter(name==select) %>%
	select(link)%>%
	as.character() %>%
	image_read() %>%
	image_resize("700")

x[x$name==select,2]%>%
	as.character()%>%
	image_read()
