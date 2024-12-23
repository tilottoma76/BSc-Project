#include <windows.h> // for MS Windows
#include <GL/glut.h> // GLUT, include glu.h and gl.h
#include <stdlib.h>
#include <GL/gl.h>
#include<vector>
#include <math.h>
# define PI 3.14159265358979323846
#include <iostream>
using namespace std;

float sun_move = 0.0f;
float cloud_move = 0.0f;
float cloud_move1 = 0.0f;
float cloud_move2 = 0.0f;
float auto_move = 0.0f;
float bird_move = 0.0f;
float boat_move = 0.0f;
float speed = 0.0f;
float savedSpeed = 0.0f;
void update(int);
float _angle1 = 30.0f;



/* Handler for window-repaint event. Call back when the window first appears and
whenever the window needs to be re-painted.
void renderBitmapString(float x, float y, float z, void *font, char *string) {
    char *c;
    glRasterPos3f(x, y,z);
    for (c=string; *c != '\0'; c++)
    {
        glutBitmapCharacter(font, *c);
    }
}*/
void myInit()
{
    glEnable(GL_DEPTH_TEST);
    gluOrtho2D(0, 1600, 0, 600);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    //glDisable(GL_LIGHT0);
    glEnable(GL_COLOR_MATERIAL);
}

bool start1 = false;
bool start2 = false;
bool start3 = false;

void keyboard(unsigned char key, int x, int y)
{
    switch (key)
    {
        case 'n': start1 = true;
        break;
        case 'd': start2 = true;
        break;
    }
}
void handleResize(int v, int h)
{
    glViewport(0, 0, v, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(45.0, (double)v / (double)h, 1.0, 200.0);
}


void circle(float x, float y, double r)
{
    float x1, y1;
    glBegin(GL_POLYGON);
    for(int i=0; i<200; i++)
    {
        float pi = 3.1416;
        float A =(i*2*pi)/25;
        float x1 = x+((r-0.07)*cos(A));
        float y1 = y+((r)*sin(A));
        glVertex2f(x1, y1);
    }
    glEnd();
}

void drawcircle(float x, float y, double r)
{
    float x1, y1;
    glBegin(GL_POLYGON);
    for(int i=0; i<200; i++)
    {
        float pi = 3.1416;
        float A =(i*2*pi)/25;
        float x1 = x+((r-0.09)*cos(A));
        float y1 = y+((r)*sin(A));
        glVertex2f(x1, y1);
    }
    glEnd();
}

void drawCircle(float x, float y, double r)
{
    float x1, y1;
    glBegin(GL_POLYGON);
    for(int i=0; i<200; i++)
    {
        float pi = 3.1416;
        float A =(i*2*pi)/25;
        float x1 = x+((r-0.03)*cos(A));
        float y1 = y+((r)*sin(A));
        glVertex2f(x1, y1);
    }
    glEnd();
}
void sky(){
    glBegin(GL_QUADS);
    glColor3ub(51, 204, 255);
    glVertex2f(-1.0f, 1.0f);
    glVertex2f(1.0f, 1.0f);
    glColor3ub(205, 240, 255);
    glVertex2f(1.0f, -0.15f);
    glVertex2f(-1.0f, -0.15f);
    glEnd();
}


void cloud1 ()
{
    glColor3f(0.8, 0.8, 0.8);
    drawCircle(-0.85, 0.77, 0.1);
    drawCircle(-0.79, 0.70, 0.1);
    glColor3f(0.9, 0.9, 0.9);
    drawCircle(-0.70, 0.70, 0.1);
    drawCircle(-0.75, 0.77, 0.1);
}

void cloud2 ()
{
    glColor3f(0.8, 0.8, 0.8);
    drawCircle(0.35, 0.77, 0.1);
    drawCircle(0.45, 0.70, 0.1);
    glColor3f(0.9, 0.9, 0.9);
    drawCircle(0.55, 0.75, 0.1);
    drawCircle(0.45, 0.80, 0.1);
}
void cloud3 ()
{
    glColor3f(0.9, 0.9, 0.9);
    drawCircle(-0.09, 0.85, 0.1);
    drawCircle(-0.02, 0.80, 0.1);
    drawCircle(0.08, 0.83, 0.1);
    drawCircle(0.02, 0.85, 0.1);
}
GLfloat position22 = 0.0f;
GLfloat speed22 = 0.007f;
void Bird(int value)
{
    if(position22 > 1.0)
        position22 =-1.0f;
    position22 += speed22;
    glutPostRedisplay();
    glutTimerFunc(100, Bird, 0);
}
void mainBird()
{
//----------1st Bird------------
    glBegin(GL_POLYGON);
    glColor3ub(97, 179, 59 );
    glVertex2f(0.1f,0.8f);
    glVertex2f(0.11f,0.79f);
    glVertex2f(0.12f,0.78f);
    glVertex2f(0.16f,0.77f);
    glVertex2f(0.19f,0.79f);
    glVertex2f(0.201f,0.8f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3ub(18, 53, 36);
    glVertex2f(0.175f,0.8f);
    glVertex2f(0.15f,0.8f);
    glVertex2f(0.14f,0.84f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3ub(18, 53, 36);
    glVertex2f(0.175f,0.8f);
    glVertex2f(0.144f,0.8f);
    glVertex2f(0.12f,0.83f);
    glEnd();

    int i;
    GLfloat m1=0.182f;//radiusmm=1,nn,mm=1
    GLfloat n1=0.801f;
    GLfloat radius1 =0.01f;
    int triangleAmount = 20;
    GLfloat twicePi = 2.0f * PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(140, 71, 54);
    glVertex2f(m1, n1);//Center of Circle
    for(i = 0; i <= triangleAmount; i++)
    {
        glVertex2f(
            m1 + (radius1 * cos(i *  twicePi / triangleAmount)),
            n1 + (radius1 * sin(i * twicePi / triangleAmount))
        );
    }
    glEnd();

//-----------2nd Bird------------
    glBegin(GL_POLYGON);
    glColor3ub(97, 179, 59);
    glVertex2f(-0.02f,0.8f);
    glVertex2f(-0.01f,0.79f);
    glVertex2f(0.0f,0.78f);
    glVertex2f(0.04f,0.77f);
    glVertex2f(0.07f,0.79f);
    glVertex2f(0.081f,0.8f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3ub(18, 53, 36);
    glVertex2f(0.055f,0.8f);
    glVertex2f(0.03f,0.8f);
    glVertex2f(0.02f,0.84f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3ub(18, 53, 36);
    glVertex2f(0.055f,0.8f);
    glVertex2f(0.024f,0.8f);
    glVertex2f(0.0f,0.83f);
    glEnd();

    GLfloat m2=0.062f;//mmm,nnn,radiusmmm=2
    GLfloat n2=0.801f;
    GLfloat radius2 =0.01f;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(140, 71, 54);
    glVertex2f(m2, n2);//Center of Circle
    for(i = 0; i <= triangleAmount; i++)
    {
        glVertex2f(
            m2 + (radius2 * cos(i *  twicePi / triangleAmount)),
            n2 + (radius2 * sin(i * twicePi / triangleAmount))
        );
    }
    glEnd();

//-----------3rd Bird---------------
    glBegin(GL_POLYGON);
    glColor3ub(79, 179, 59);
    glVertex2f(-0.72f,0.8f);
    glVertex2f(-0.71f,0.79f);
    glVertex2f(-0.7f,0.78f);
    glVertex2f(-0.66f,0.77f);
    glVertex2f(-0.63f,0.79f);
    glVertex2f(-0.619f,0.8f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3ub(18, 53, 36);
    glVertex2f(-0.645f,0.8f);
    glVertex2f(-0.67f,0.8f);
    glVertex2f(-0.68f,0.84f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3ub(18, 53, 36);
    glVertex2f(-0.645f,0.8f);
    glVertex2f(-0.676f,0.8f);
    glVertex2f(-0.7f,0.83f);
    glEnd();

    GLfloat m3=-0.638f;//mmmm,nnnn=3,radiusmmm=2
    GLfloat n3=0.801f;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(140, 71, 54);
    glVertex2f(m3,n3);//Center of Circle
    for(i = 0; i <= triangleAmount; i++)
    {
        glVertex2f(
            m3 + (radius2 * cos(i *  twicePi / triangleAmount)),
            n3 + (radius2 * sin(i * twicePi / triangleAmount))
        );
    }
    glEnd();

//--------------4th bird-------------
    glBegin(GL_POLYGON);
    glColor3ub(79, 179, 59);
    glVertex2f(-0.6f,0.8f);
    glVertex2f(-0.59f,0.79f);
    glVertex2f(-0.58f,0.78f);
    glVertex2f(-0.54f,0.77f);
    glVertex2f(-0.51f,0.79f);
    glVertex2f(-0.499f,0.8f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3ub(18, 53, 36);
    glVertex2f(-0.525f,0.8f);
    glVertex2f(-0.55f,0.8f);
    glVertex2f(-0.56f,0.84f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3ub(18, 53, 36);
    glVertex2f(-0.525f,0.8f);
    glVertex2f(-0.556f,0.8f);
    glVertex2f(-0.58f,0.83f);
    glEnd();

    GLfloat m4=-0.518f;//mmmmm,nnnnn=4, radiusmm=1
    GLfloat n4=0.801f;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(140, 71, 54);
    glVertex2f(m4, n4);//Center of Circle
    for(i = 0; i <= triangleAmount; i++)
    {
        glVertex2f(
            m4 + (radius1 * cos(i *  twicePi / triangleAmount)),
            n4 + (radius1 * sin(i * twicePi / triangleAmount))
        );
    }
    glEnd();
}

void road(){
    glBegin(GL_QUADS);
    glColor3f(0.5f, 0.5f, 0.50f);
    glVertex2f(-1.0f, -0.18f);
    glVertex2f(1.0f, -0.18f);
    glColor3f(0.4f, 0.4f, 0.4f);
    glVertex2f(1.0f, -0.35f);
    glVertex2f(-1.0f, -0.35f);
    glEnd();

}
void Sun()
{
    glColor3f(1.0f, 0.6f, 0.07f);
    circle(-0.25, 0.75,0.16);
}
void Moon()
{
    glPushMatrix();
    glColor3f(1.0,1.0,1.0);
    circle(0.75, 0.75, 0.16);
    glPopMatrix();
}

void house1()
{
    glBegin(GL_POLYGON);//roof
    glColor3f(0.33f, 0.19f, 0.0f);
    glVertex2f(-0.71, 0.20);
    glVertex2f(-0.64, 0.34);
    glVertex2f(-0.38, 0.34);
    glVertex2f(-0.31, 0.20);
    glEnd();

    glColor3f(0.5f, 0.2f, 0.9f);
    glBegin(GL_QUADS);
    glVertex2f(-0.36, -0.06);
    glVertex2f(-0.36, 0.20);
    glVertex2f(-0.66, 0.20);
    glVertex2f(-0.66, -0.06);
    glEnd();

    glColor3f(0.33f, 0.19f, 0.f);//window
    glBegin(GL_QUADS);
    glVertex2f(-0.64, 0.15);
    glVertex2f(-0.64, 0.08);
    glVertex2f(-0.58, 0.08);
    glVertex2f(-0.58, 0.15);
    glEnd();

    glColor3f(0.33f, 0.19f, 0.0f);//door
    glBegin(GL_QUADS);
    glVertex2f(-0.55, 0.17);
    glVertex2f(-0.55, -0.06);
    glVertex2f(-0.48, -0.06);
    glVertex2f(-0.48, 0.17);
    glEnd();

    glColor3f(0.33f, 0.19f, 0.0f); //window 3
    glBegin(GL_QUADS);
    glVertex2f(-0.45, 0.15);
    glVertex2f(-0.45, 0.08);
    glVertex2f(-0.38, 0.08);
    glVertex2f(-0.38, 0.15);
    glEnd();
    glColor3f(0.33f, 0.19f, 0.0f);//door
    glBegin(GL_QUADS);
    glVertex2f(-0.34, -0.10);
    glVertex2f(-0.34, -0.06);
    glVertex2f(-0.68, -0.06);
    glVertex2f(-0.68, -0.10);
    glEnd();


}
void house2()
{
    glBegin(GL_POLYGON);//roof
    glColor3f(0.0f, 0.0f, 0.54f);
    glVertex2f(0.71, 0.27);
    glVertex2f(0.64, 0.41);
    glVertex2f(0.38, 0.41);
    glVertex2f(0.31, 0.27);
    glEnd();

    glColor3f(0.4f, 0.6f, 0.9f); //body
    glBegin(GL_QUADS);
    glVertex2f(0.36, 0.01);
    glVertex2f(0.36, 0.27);
    glVertex2f(0.66, 0.27);
    glVertex2f(0.66, 0.01);
    glEnd();

    glColor3f(0.0f, 0.0f, 0.54f);//window
    glBegin(GL_QUADS);
    glVertex2f(0.64, 0.22);
    glVertex2f(0.64, 0.15);
    glVertex2f(0.58, 0.15);
    glVertex2f(0.58, 0.22);
    glEnd();

    glColor3f(0.0f, 0.0f, 0.54f);//door
    glBegin(GL_QUADS);
    glVertex2f(0.55, 0.24);
    glVertex2f(0.55, 0.01);
    glVertex2f(0.48, 0.01);
    glVertex2f(0.48, 0.24);
    glEnd();

    glColor3f(0.0f, 0.0f, 0.54f); //window 3
    glBegin(GL_QUADS);
    glVertex2f(0.45, 0.22);
    glVertex2f(0.45, 0.15);
    glVertex2f(0.38, 0.15);
    glVertex2f(0.38, 0.22);
    glEnd();
    glColor3f(0.0f, 0.0f, 0.54f);//door
    glBegin(GL_QUADS);
    glVertex2f(0.34, -0.03);
    glVertex2f(0.34, 0.01);
    glVertex2f(0.68, 0.01);
    glVertex2f(0.68, -0.03);
    glEnd();
}

void house3()
{
    /*glBegin(GL_QUADS);
    glColor3ub(73,73,73);
    glVertex2f(0.05, 0.1);
    glVertex2f(0.1, 0.2);
    glVertex2f(-0.15, 0.2);
    glVertex2f(-0.1, 0.1);
    glEnd();*/
}

void autoRickshaw()
{
    glBegin(GL_POLYGON);
    glColor3f(0.5, 0.0, 0.0);
    glVertex2f(0.60, -0.25);
    glVertex2f(0.75, -0.25);
    glColor3f(0.0, 0.0, 0.0);
    glVertex2f(0.75, -0.02);
    glVertex2f(0.60, -0.02);
    glEnd();
    glBegin(GL_POLYGON);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor3f(0.5, 0.5, 0.5);
    glVertex2f(0.62, -0.21);
    glVertex2f(0.68, -0.21);
    glColor3f(0.4, 0.8, 0.0);
    glVertex2f(0.68, -0.06);
    glVertex2f(0.62, -0.06);
    glDisable(GL_BLEND);
    glEnd();

    glBegin(GL_POLYGON);
    glColor3f(0.5, 0.0, 0.0);
    glVertex2f(0.50, -0.25);
    glVertex2f(0.60, -0.25);
    glColor3f(0.0, 0.0, 0.0);
    glVertex2f(0.60, -0.02);
    glVertex2f(0.55, -0.04);
    glVertex2f(0.50, -0.07);
    glEnd();

    glBegin(GL_POLYGON);
    glColor3f(0.4, 0.8, 0.0);
    glVertex2f(0.52, -0.16);
    glVertex2f(0.58, -0.16);
    glColor3f(0.4, 0.8, 0.0);
    glVertex2f(0.58, -0.06);
    glVertex2f(0.53, -0.08);
    glVertex2f(0.52, -0.09);
    glEnd();

    glColor3f(0.0, 0.0, 0.0);
    drawCircle(0.57, -0.1, 0.02);
    glBegin(GL_TRIANGLES);
    glColor3f(0.0, 0.0, 0.0);
    glVertex2f(0.57, -0.1);
    glVertex2f(0.55, -0.16);
    glVertex2f(0.58, -0.16);
    glEnd();


    glColor3f(0.0, 0.0, 0.0);
    drawCircle(0.50, -0.25, 0.07);
    drawCircle(0.75, -0.25, 0.06);
    glColor3f(1.0, 1.0, 1.0);
    drawCircle(0.50, -0.25, 0.02);
    drawCircle(0.75, -0.25, 0.02);


}
void grass()
{
    glPushMatrix();
    glBegin(GL_POLYGON);
    glColor3f(0.4f, 0.8f, 0.0f); //grass
    glVertex2f(1.0f, -0.0f);
    glVertex2f(1.0f, -1.0f);
    glVertex2f( -1.0f,  -1.0f);
    glVertex2f(-1.0f,  -0.0f);
    glEnd();
    circle(0.5, -0.3, 0.6);
    circle(0.0, -0.2, 0.4);
    circle(-0.6, -0.4, 0.55);

}
void boat()
{

    glColor3f(0.4, 0.2, 0.0);
    circle(-0.7, -0.75, 0.1);
    glColor3f(0.0, 0.0, 0.5);
    drawcircle(-0.45, -0.77, 0.07);
    glBegin(GL_POLYGON);
    glColor3f(0.4, 0.2, 0.0);
    glVertex2f(-0.7, -0.8);
    glVertex2f(-0.5, -0.8);
    glVertex2f(-0.5, -0.65);
    glVertex2f(-0.7, -0.65);
    glEnd();

    glColor3f(0.64, 0.45, 0.3);
    circle(-0.5, -0.75, 0.1);
    glBegin(GL_POLYGON);
    glColor3f(0.3, 0.074, 0.074);
    glVertex2f(-0.75, -0.9);
    glVertex2f(-0.45, -0.9);
    glVertex2f(-0.4, -0.8);
    glVertex2f(-0.8, -0.8);
    glEnd();
    glColor3f(0.0, 0.0, 0.0);
    drawCircle(-0.45, -0.67, 0.045);

    glBegin(GL_POLYGON);
    glColor3f(0.0, 0.0, 0.0);
    glVertex2f(-0.47, -0.75);
    glVertex2f(-0.48, -0.73);
    glVertex2f(-0.44, -0.92);
    glVertex2f(-0.43, -0.92);
    glEnd();

}
void river()
{
    glBegin(GL_POLYGON);
    glColor3f(0.54f, 0.41f, 0.35f);
    glVertex2f(1.0f, -0.58f);
    glVertex2f(1.0f, -0.6f);
    glVertex2f(-1.0f, -0.6f);
    glVertex2f(-1.0f, -0.58f);
    glEnd();

    glBegin(GL_POLYGON);
    glColor3f(0.0f, 0.75f, 1.0f);
    //glColor3f(0.54f, 0.41f, 0.35f);
    glVertex2f(1.0f, -0.6f);
    glVertex2f(1.0f, -1.0f);
    glColor3f(0.0f, 0.66f, 1.0f);
    glVertex2f(-1.0f, -1.0f);
    glVertex2f(-1.0f, -0.6f);
    glEnd();

    glBegin(GL_POLYGON);
    glColor3f(0.0f, 0.66f, 1.0f);
    glVertex2f(1.0f, -0.7f);
    glVertex2f(1.0f, -1.0f);
    glColor3f(0.0f, 0.6f, 1.0f);
    glVertex2f(-1.0f, -1.0f);
    glVertex2f(-1.0f, -0.7f);
    glEnd();

    glBegin(GL_POLYGON);
    glColor3f(0.0f, 0.6f, 1.0f);
    glVertex2f(1.0f, -0.8f);
    glVertex2f(1.0f, -1.0f);
    glColor3f(0.0f, 0.66f, 1.0f);
    glVertex2f(-1.0f, -1.0f);
    glVertex2f(-1.0f, -0.8f);
    glEnd();

    glBegin(GL_POLYGON);
    glColor3f(0.0f, 0.66f, 1.0f);
    glVertex2f(1.0f, -0.9f);
    glVertex2f(1.0f, -1.0f);
    glColor3f(0.0f, 0.75f, 1.0f);
    glVertex2f(-1.0f, -1.0f);
    glVertex2f(-1.0f, -0.9f);
    glEnd();

}

void tree1()
{
     glBegin(GL_POLYGON);
    glColor3f(0.6f, 0.3f, 0.0f); //tree body
    glVertex2f(-0.80f, -0.1f);
    glVertex2f(-0.80f, 0.17f);
    glVertex2f( -0.85f,  0.17f);
    glVertex2f(-0.85f,  -0.1f);
    glEnd();

    glBegin(GL_POLYGON);
    glColor3f(0.6f, 0.3f, 0.0f); //tree body
    glVertex2f(-0.86f, -0.1f);
    glVertex2f(-0.87f, -0.15f);
    glVertex2f( -0.83f,  -0.1f);
    glVertex2f(-0.83f,  -0.05f);
    glEnd();

    glBegin(GL_POLYGON);
    glColor3f(0.6f, 0.3f, 0.0f); //tree body
    glVertex2f(-0.81f, -0.05f);
    glVertex2f(-0.83f, -0.1f);
    glVertex2f( -0.78f,  -0.13f);
    glVertex2f(-0.80f,  -0.1f);
    glEnd();

    glColor3f(0.2f, 0.8f, 0.2f);
    drawcircle(-0.78f, 0.15f, 0.06f);
    glColor3f(0.4f, 0.8f, 0.0f); //grass
    drawcircle(-0.82f, 0.13f, 0.06f);
    glColor3f(0.2f, 0.8f, 0.2f);
    drawcircle(-0.86f, 0.15f, 0.06f);
    glColor3f(0.4f, 0.8f, 0.0f); //grass
    drawcircle(-0.87f, 0.19f, 0.06f);
    glColor3f(0.2f, 0.8f, 0.2f);
    drawcircle(-0.78f, 0.24f, 0.06f);
    glColor3f(0.4f, 0.8f, 0.0f); //grass
    drawcircle(-0.82f, 0.23f, 0.06f);
    glColor3f(0.2f, 0.8f, 0.2f);
    drawcircle(-0.84f, 0.28f, 0.06f);
    glColor3f(0.4f, 0.8f, 0.0f); //grass
    drawcircle(-0.87f, 0.24f, 0.06f);
    glColor3f(0.2f, 0.8f, 0.2f);
    drawcircle(-0.80f, 0.28f, 0.06f);
}
void tree2()
{
     glBegin(GL_POLYGON);
    glColor3f(0.6f, 0.3f, 0.0f); //tree body
    glVertex2f(0.80f, -0.1f);
    glVertex2f(0.80f, 0.17f);
    glVertex2f(0.85f,  0.17f);
    glVertex2f(0.85f,  -0.1f);
    glEnd();

    glBegin(GL_POLYGON);
    glColor3f(0.6f, 0.3f, 0.0f); //tree body
    glVertex2f(0.86f, -0.1f);
    glVertex2f(0.87f, -0.15f);
    glVertex2f(0.83f,  -0.1f);
    glVertex2f(0.83f,  -0.05f);
    glEnd();

    glBegin(GL_POLYGON);
    glColor3f(0.6f, 0.3f, 0.0f); //tree body
    glVertex2f(0.81f, -0.05f);
    glVertex2f(0.83f, -0.1f);
    glVertex2f(0.78f,  -0.13f);
    glVertex2f(0.80f,  -0.1f);
    glEnd();

    glColor3f(0.2f, 0.8f, 0.2f);
    drawcircle(0.78f, 0.15f, 0.06f);
    glColor3f(0.4f, 0.8f, 0.0f); //grass
    drawcircle(0.82f, 0.13f, 0.06f);
    glColor3f(0.2f, 0.8f, 0.2f);
    drawcircle(0.86f, 0.15f, 0.06f);
    glColor3f(0.4f, 0.8f, 0.0f); //grass
    drawcircle(0.87f, 0.19f, 0.06f);
    glColor3f(0.2f, 0.8f, 0.2f);
    drawcircle(0.78f, 0.24f, 0.06f);
    glColor3f(0.4f, 0.8f, 0.0f); //grass
    drawcircle(0.82f, 0.23f, 0.06f);
    glColor3f(0.2f, 0.8f, 0.2f);
    drawcircle(0.84f, 0.28f, 0.06f);
    glColor3f(0.4f, 0.8f, 0.0f); //grass
    drawcircle(0.87f, 0.24f, 0.06f);
    glColor3f(0.2f, 0.8f, 0.2f);
    drawcircle(0.80f, 0.28f, 0.06f);

}

void night_effect()
{
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f); // Set background color to black and opaque
    glClear(GL_COLOR_BUFFER_BIT);

    glPushMatrix();
    glPointSize(6.0);
    glBegin(GL_POINTS);
    glColor3f(1.0, 1.0, 1.0);
    glVertex2f(-0.9f, 0.9f);
    glVertex2f(-0.8f,0.8f);
    glVertex2f(-0.9f,0.71f);
    glVertex2f(-0.82f,0.7f);
    glVertex2f(-0.7f, 0.6f);
    glVertex2f(-0.65f, 0.9f);

    glVertex2f(0.9f, 0.9f);
    glVertex2f(0.8f,0.8f);
    glVertex2f(0.9f,0.55f);
    glVertex2f(0.82f,0.4f);
    glVertex2f(0.7f, 0.6f);
    glVertex2f(0.65f, 0.9f);

    glVertex2f(0.0f, 0.5f);
    glVertex2f(0.0f, 0.65f);


    glVertex2f(-0.55f, 0.9f);
    glVertex2f(-0.45f,0.6f);
    glVertex2f(-0.4f,0.72f);
    glVertex2f(-0.33f,0.63f);
    glVertex2f(-0.33f,0.4f);
    glVertex2f(-0.22f, 0.45f);
    glVertex2f(-0.1f, 0.9f);

    glVertex2f(0.55f, 0.9f);
    glVertex2f(0.45f,0.6f);
    glVertex2f(0.4f,0.72f);
    glVertex2f(0.33f,0.63f);
    glVertex2f(0.33f,0.4f);
    glVertex2f(0.22f, 0.45f);
    glVertex2f(0.1f, 0.9f);

    glEnd();

    /*glBegin(GL_POLYGON);
    glColor3f(0.004f, 0.2f, 0.64f); //grass
    glVertex2f(1.0f, -0.0f);
    glVertex2f(1.0f, -1.0f);
    glVertex2f( -1.0f,  -1.0f);
    glVertex2f(-1.0f,  -0.0f);
    glEnd();*/
}

void update(int value)
{
    glutPostRedisplay();
    glutTimerFunc(25, update, 0);
}



void display() {
    glClearColor(0.88f, 1.0f, 1.0f, 0.0f); // Set background color to black and opaque
    glClear(GL_COLOR_BUFFER_BIT); // Clear the color buffer (background)
    glLoadIdentity();
    glMatrixMode(GL_MODELVIEW);

    sky();
    glPushMatrix();
    glTranslatef(0.0, sun_move,0.0);
    Sun();
    glPopMatrix();
    glPushMatrix();
    glTranslatef(cloud_move, 0.0,0.0);
    cloud1();
    glPopMatrix();

    glPushMatrix();
    glTranslatef(cloud_move1, 0.0,0.0);
    cloud2();
    glPopMatrix();

    glPushMatrix();
    glTranslatef(cloud_move2, 0.0,0.0);
    cloud3();
    glPopMatrix();

    cloud_move += 0.003;
    {
        if(cloud_move > 1.9)
        {
            cloud_move = -1.7;
        }
    }

    cloud_move1 += 0.003;
    {
        if(cloud_move1 > 1.9)
        {
            cloud_move1 = -1.7;
        }
    }

    cloud_move2 += 0.003;
    {
        if(cloud_move2 > 1.9)
        {
            cloud_move2 = -1.7;
        }
    }
    glEnd();

    if (start3 == true)
    {
        glEnable(GL_LIGHT0);
        night_effect();
        Moon();
    }

    if (start1 == true)
    {
        sun_move -= 0.005;
        if(sun_move < -0.95)
        {
            glDisable(GL_LIGHT0);
            start1=false;
            start3=true;
        }
    }
    if (start2 == true)
    {
        start3=false;
        glEnable(GL_LIGHT0);
        sun_move += 0.005;
        if (sun_move > 0.30)
        {
            glEnable(GL_LIGHT0);
        }
        if (sun_move > 0)
        {
            start2= false;
        }
    }


    glPushMatrix();
    glTranslatef(bird_move, 0.0,0.0);
    mainBird();
    glPopMatrix();
    bird_move += 0.009;
    {
        if(bird_move > 1.9)
        {
            bird_move = -1.7;
        }
    }
    glEnd();

    grass();

    road();
    tree1();
    tree2();
    house1();
    house2();
    house3();
    glPushMatrix();
    glTranslatef(auto_move, 0.0, 0.0);
    autoRickshaw();
    glPopMatrix();
    auto_move -= 0.004;
    if (auto_move < - 1.9)
    {
        auto_move = 1.0;
    }
    glEnd();

    glColor3f(0.4f, 0.8f, 0.0f); //grass
    river();
    glPushMatrix();
    glTranslatef(boat_move, 0.0,0.0);
    boat();
    glPopMatrix();
    boat_move += 0.005;
    {
        if(boat_move > 1.9)
        {
            boat_move = -1.7;
        }
    }
    glEnd();



     glutSwapBuffers();
}



/* Main function: GLUT runs as a console application starting at main() */
int main(int argc, char** argv)
{


    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
    myInit();
    glutInitWindowSize(1200,600);
    glutInitWindowPosition(100,10);

    glutCreateWindow("Nature");
    glutDisplayFunc(display);
    glutReshapeFunc(handleResize);

    glutKeyboardFunc(keyboard);

	glutTimerFunc(25, update, 0);
    glutMainLoop();
    return 0;
}
