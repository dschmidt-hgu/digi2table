/*** Include Files***/
#include "shapes.inc"
#include "colors.inc"

/*** Camera ***/
camera {
 location    <250,140, 0>
 look_at     <0, 100, 0>
 right       x*image_width/image_height
}

/*** Light source ***/
#declare Dist=3000.0;
light_source {
     <210, 550, -190> color White
     fade_distance Dist fade_power 4
}

/*** Texture for vegetative plant parts ***/
#declare Color_Fruit=texture{
     pigment{color ForestGreen}
}
#declare Color_Fruit=
     pigment{color ForestGreen}
#declare Color_Fruit_Stem=texture {
     pigment{color ForestGreen}
}
#declare Color_Leaf=texture {
#declare Color_Leaf=
     pigment{color rgb <0.5, 0.8, 0.1>}
}
#declare Color_Petiole=texture {
     pigment{color rgb <0.5,  0.8, 0.1>}
}
#declare Color_Internode=texture {
     pigment{color rgb <0.5, 0.8, 0.1>}
}

/*** Atmosphere ***/
sky_sphere {
 pigment {
     gradient y
     color_map { [0.0 color rgb <0.7, 0.7, 1.0>] [1.0 color blue 0.5] }
 }
}

/*** Coordinate-Axes ***/
//East (X coordinate)
#declare East_X_Coordinate = mesh {
triangle { <0,-0.5,-0.5>,<0,-0.5,0.5>, <100,-0.5,-0.5>}
triangle {<0,-0.5,0.5>, <100,-0.5,0.5>, <100,-0.5,-0.5>}
triangle { <0,-0.5,0.5>, <0,0.5,0>, <100,-0.5,0.5>}
triangle { <0,0.5,0>, <100,0.5,0>, <100,-0.5,0.5>}
triangle { <0,0.5,0>, <0,-0.5,-0.5>, <100,0.5,0>}
triangle { <0,-0.5,-0.5>, <0,-0.5,-0.5>, <100,0.5,0>}
texture{pigment{color Blue}}}

// North (Z coordinate)
#declare North_Z_Coordinate = mesh {
triangle { <-0.5,-0.5,0>,<-0.5,0.5,0>, <-0.5,-0.5,100>}
triangle {<-0.5,0.5,0>, <-0.5,0.5,100>, <-0.5,-0.5,100>}
triangle { <-0.5,0.5,0>, <0.5,0,0>, <-0.5,0.5,100>}
triangle { <0.5,0,0>, <0.5,0,100>, <-0.5,0.5,100>}
triangle { <0.5,0,0>, <-0.5,-0.5,0>, <0.5,0,100>}
triangle { <-0.5,-0.5,0>, <-0.5,-0.5,100>, <0.5,0,100>}
texture{pigment{color Red}}}

// Vertical direction (Y coordinate)
#declare Vertical_Direction_Y_Coordinate = mesh {
triangle { <-0.5,0,-0.5>,<-0.5,0,0.5>, <-0.5,150,-0.5>}
triangle {<-0.5,0,0.5>, <-0.5,150,0.5>, <-0.5,150,-0.5>}
triangle { <-0.5,0,0.5>, <0.5,0,0>, <-0.5,150,0.5>}
triangle { <0.5,0,0>, <0.5,150,0>, <-0.5,150,0.5>}
triangle { <0.5,0,0>, <-0.5,0,-0.5>, <0.5,150,0>}
triangle { <-0.5,0,-0.5>, <-0.5,150,-0.5>, <0.5,150,0>}
texture{pigment{color Orange}}}

East_X_Coordinate
North_Z_Coordinate
Vertical_Direction_Y_Coordinate

/**** Leaf ID: 0 ****/

#declare leaf_id_0 = mesh {
triangle { <-2.91, 0.03000000000000025, 0.86>, <-6.709999999999997, 0.07000000000000028, 0.8200000000000001>, <-7.66, 0.0600000000000005, 0.8>}
triangle { <-2.91, 0.03000000000000025, 0.86>, <-9.579999999999998, 0.0600000000000005, 0.77>, <-10.509999999999998, 0.11000000000000032, 0.73>}
triangle { <-2.91, 0.03000000000000025, 0.86>, <-5.789999999999999, 0.03000000000000025, 0.81>, <-6.709999999999997, 0.07000000000000028, 0.8200000000000001>}
triangle { <-2.91, 0.03000000000000025, 0.86>, <-8.639999999999997, 0.040000000000000036, 0.75>, <-9.579999999999998, 0.0600000000000005, 0.77>}
triangle { <-2.91, 0.03000000000000025, 0.86>, <-3.849999999999998, -0.0, 0.87>, <-5.789999999999999, 0.03000000000000025, 0.81>}
triangle { <-2.91, 0.03000000000000025, 0.86>, <-3.849999999999998, -0.0, 0.87>, <-8.639999999999997, 0.040000000000000036, 0.75>}
triangle { <-3.849999999999998, -0.0, 0.87>, <-4.779999999999998, -0.0, 0.83>, <-5.789999999999999, 0.03000000000000025, 0.81>}
triangle { <-3.849999999999998, -0.0, 0.87>, <-4.779999999999998, -0.0, 0.83>, <-8.639999999999997, 0.040000000000000036, 0.75>}
    texture {pigment{color ForestGreen}}}

leaf_id_0

#declare leaf_id_0_petiole = mesh{
triangle { <0.0, -0.0, 0.0>, < 0.5, -0.0, 0.0>, < 0.5, -0.0, 0.0>}
triangle { <0.0, -0.0, 0.0>, < 0.5, -0.0, 0.0>, < 0.0, -0.0, 0.0>}
triangle { <0.0, -0.0, 0.5>, < 0.0, -0.0, 0.5>, < 0.0, -0.0, 0.0>}
triangle { <0.5, -0.0, 0.0>, < 0.5, -0.0, 0.0>, < 0.0, -0.0, 0.5>}
triangle { <0.5, -0.0, 0.0>, < 0.0, -0.0, 0.5>, < 0.0, -0.0, 0.5>}
triangle { <0.0, -0.0, 0.0>, < 0.5, -0.0, 0.0>, < 0.0, -0.0, 0.5>}
triangle { <0.0, -0.0, 0.0>, < 0.5, -0.0, 0.0>, < 0.0, -0.0, 0.5>}
triangle { <0.0, -0.0, 0.0>, < 0.5, -0.0, 0.0>, < -2.41, 0.03000000000000025, 0.86>}
triangle { <0.0, -0.0, 0.0>, < -2.41, 0.03000000000000025, 0.86>, < -2.91, 0.03000000000000025, 0.86>}
triangle { <0.0, -0.0, 0.5>, < -2.91, 0.03000000000000025, 1.3599999999999999>, < -2.91, 0.03000000000000025, 0.86>}
triangle { <0.5, -0.0, 0.0>, < -2.41, 0.03000000000000025, 0.86>, < -2.91, 0.03000000000000025, 1.3599999999999999>}
triangle { <0.5, -0.0, 0.0>, < -2.91, 0.03000000000000025, 1.3599999999999999>, < 0.0, -0.0, 0.5>}
triangle { <0.0, -0.0, 0.0>, < 0.5, -0.0, 0.0>, < 0.0, -0.0, 0.5>}
triangle { <-2.91, 0.03000000000000025, 0.86>, < -2.41, 0.03000000000000025, 0.86>, < -2.91, 0.03000000000000025, 1.3599999999999999>}
    texture {pigment{color rgb<1.00, 0.050, 0.25>}}}

leaf_id_0_petiole
