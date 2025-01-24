@echo off
cd data-raw/overture_schema
IF EXIST schema rmdir /s /q schema
git clone https://github.com/OvertureMaps/schema.git