# ##########################################
# # THIS WAS DISCONTINUED AFTER A MANUAL APPROACH WAS DISCOVERED TO SAVE SO MUCH TIME.
# ##########################################
# ##########################################
#
#
# #####################
# # This python script imports two raster data files, grids them, and creates the necessary centroid locations
# #####################
#
# # INITIALISE
# #####################
#
# import sys
# sys.path.append('/Applications/QGIS.app/Contents/Resources/python/')
# sys.path.append('/Applications/QGis.app/Contents/Resources/python/plugins')
#
#
# from qgis.core import *
# from PyQt4.QtCore import QFileInfo
# app = QgsApplication([],True)
# QgsApplication.setPrefixPath("/Applications/QGIS.app/Contents/MacOS", True)
# QgsApplication.initQgis()
#
# import processing
# from processing.core.Processing import Processing
# from processing.tools import *
#
#
# Processing.initialize()
# Processing.updateAlgsList()
#
#
# # LOAD PROJECT
#
# project = QgsProject.instance()
# project.read(QFileInfo('/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/GIS_Project/GIS_environment.qgs'))
#
# # LOAD DATA
# ######################
#
#
# # PRINT ALL COUNTRY NAMES
#
# # iter = countries.getFeatures()
# # for feature in iter:
# #     print feature['NAME']
#
#
# # LOAD LIGHTS DATA
# #####################
#
# fileName = "/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/light_raster.tif"
# fileInfo = QFileInfo(fileName)
# baseName = fileInfo.baseName()
# rlayer = QgsRasterLayer(fileName, baseName)
# if not rlayer.isValid():
#   print "Layer failed to load!"
#
# #print rlayer.extent().toString()
#
#
# # CREATE GRID -- Did not work
# #
# # cellsize = 0.5000 #Cell Size in WGS 84 will be 50 x 50 Kilometers
# # crs = "EPSG:4326" #WGS 84 System
# # xmin = -180 #extract the minimum x coord from our layer
# # xmax =  180 #extract our maximum x coord from our layer
# # ymin = -60 #extract our minimum y coord from our layer
# # ymax =  60 #extract our maximum y coord from our layer
# # #prepare the extent in a format the VectorGrid tool can interpret (xmin,xmax,ymin,ymax)
# # extent = str(xmin)+ ',' + str(xmax)+ ',' +str(ymin)+ ',' +str(ymax)
# # grid="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/GIS_Project"
# # processing.runalg('qgis:vectorgrid',  extent, cellsize, cellsize,  0, grid)
# # QgsVectorFileWriter.writeAsVectorFormat(grids, folder_name+'/pos', "System", None,"ESRI Shapefile")
#
#
# # Instead: IMPORT GRID
#
# grid = QgsVectorLayer("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/GIS_Project/Grid.shp", "Grid", "ogr")
# if not grid.isValid():
#  print "Layer failed to load!"
#
# # Could i just do max of population in each grid (I know I can do that) and then put the xy coordinates where it matches the max?
#
# # LOAD COUNTRY SHAPEFILE
#
# countries = QgsVectorLayer("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/World_Countries/TM_WORLD_BORDERS-0.3.shp", "Countries", "ogr")
# if not countries.isValid():
#  print "Layer failed to load!"
#
#
# # This selects country by name
# query = '"NAME" == "Nigeria"'
# selection = countries.getFeatures(QgsFeatureRequest().setFilterExpression(query))
# countries.setSelectedFeatures([k.id() for k in selection])
#
# processing.runalg("qgis:clip",grid,countries,"Nigeria1.shp")
