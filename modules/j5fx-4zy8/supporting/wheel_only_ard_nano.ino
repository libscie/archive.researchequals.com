#include<Servo.h>
Servo servo4;     // Servo 4 = running-wheel brake                                                   
//Constants
const int slowness = 500;   //slowness factor, us wait between 1deg movements, change to set door speed
//servo pin
const int servoPin4 = 9;// brake servo in running wheel
//break servo angles: determine empirically
const int RELEASE_WHEEL = 80;//Angle of 80 degrees -> WHEEL is free
const int BRAKE_WHEEL = 110;//CLAMPED
//command inputs from Pi
const int pi_ard_4ow = 1;//release running wheel
//Variables
int pos4_current = RELEASE_WHEEL; //initial position variables for servos
int pos4_target = RELEASE_WHEEL;

void setup()
{
  pinMode(pi_ard_4ow, INPUT);
  servo4.attach(servoPin4);
  servo4.write(BRAKE_WHEEL);
  delay(500);
  servo4.write(RELEASE_WHEEL);
  delay(100);
}

void loop()
{   
    //servo movement loops
    if (pos4_current<pos4_target) //SERVO 4
    {
        pos4_current=pos4_current+1;
        servo4.write(pos4_current);     
        delayMicroseconds(slowness); 
    }
    if (pos4_current>pos4_target)
    {
        pos4_current=pos4_current-1;
        servo4.write(pos4_current);     
        delayMicroseconds(slowness); 
    }
    //target commands
    //SERVO 4
    if ((digitalRead(pi_ard_4ow) == HIGH)) 
    {
        pos4_target=RELEASE_WHEEL;
    }
    else 
    {
        pos4_target=BRAKE_WHEEL;
    }
}//void loop end
