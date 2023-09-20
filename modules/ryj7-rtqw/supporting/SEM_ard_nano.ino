#include<Servo.h>
Servo servo1;     // Servo 1 = door1
#include<Servo.h>
Servo servo2;     // Servo 2 = door2
                                             
//Constants
const int slowness = 8000;   //slowness factor, us wait between 1deg movements, change to set door speed
//beam breaks
const int pResistor1 = A0;//BB1 safety
const int pResistor2 = A1;//BB2 return

//servo control
const int servoPin1 = 9;// door servos 1-2
const int servoPin2 = 10;

//outputs to Pi
const int ard_pi_BB1 = 4;//reports BB1low to Pi
const int ard_pi_BB2 = 5;//reports BB2low to Pi

//door angles: determine empirically
const int CLOSE_DOOR1 = 179;//Angle of 179 degrees -> door is closed
const int OPEN_DOOR1 = 76;//Angle of 76 degrees -> door is opened
const int CLOSE_DOOR2 = 179;//Angle of 179 degrees -> door is closed
const int OPEN_DOOR2 = 45;//Angle of 45 degrees -> door is opened

//motor inputs from Pi
const int pi_ard_door1 = 2;//open door1
const int pi_ard_door2 = 3;//open door2

//Variables
int photo_value1;//Store value from photoresistor (0-1023)
int INIT_READ1;//Store initial value from photoresistor    
int photo_value2;//Store value from photoresistor (0-1023)
int INIT_READ2;
int pos1_current = CLOSE_DOOR1; //initial position variables for servos
int pos1_target = CLOSE_DOOR1;
int pos2_current = CLOSE_DOOR2; //initial position variables for servos
int pos2_target = CLOSE_DOOR2;
long start = millis();

  
void setup()
{
//  Serial.begin(9600);//setup serial
  pinMode(pResistor1, INPUT);//Set photoResistor - A0 pin as an input 
  pinMode(pResistor2, INPUT);
  pinMode(ard_pi_BB1, OUTPUT);//output reports to Pi
  pinMode(ard_pi_BB2, OUTPUT);  
  pinMode(pi_ard_door1, INPUT);//input commands from Pi
  pinMode(pi_ard_door2, INPUT);

  servo1.attach(servoPin1);
  servo1.write(OPEN_DOOR1);
  delay(1500);
  INIT_READ1 = analogRead(pResistor1);//calibrate top of door detector when door1 open
  delay(200);
  servo1.write(CLOSE_DOOR1); 
  delay(1000);
  servo2.attach(servoPin2);
  servo2.write(OPEN_DOOR2);
  delay(1000);
  servo2.write(CLOSE_DOOR2);
  delay(1000);
  
  digitalWrite(ard_pi_BB1, LOW);//communication to Pi
  digitalWrite(ard_pi_BB2, LOW);
  
  INIT_READ2 = analogRead(pResistor2);// calibrate exit beam-break

//  Serial.print("INIT_READ1 ");  //show beam break values for trouble shooting if serial monitor is on
//  Serial.println(INIT_READ1);  
//  Serial.print("INIT_READ2 ");  
//  Serial.println(INIT_READ2); 
}

void loop()
{
    photo_value1 = analogRead(pResistor1); //read beam breaks
    photo_value2 = analogRead(pResistor2);

//    Serial.print("photo_value1 ");  
//    Serial.println(photo_value1);  
//    Serial.print("photo_value2 ");  
//    Serial.println(photo_value2);  

    //output BB detectors to Pi
    if (photo_value1<INIT_READ1*0.85) //BB1 safety - inverted here
    {
        digitalWrite(ard_pi_BB1, LOW);
    }  
    else
    {
        digitalWrite(ard_pi_BB1, HIGH);
    }

    if (photo_value2<INIT_READ2*0.9) //BB2 exit
    {
        digitalWrite(ard_pi_BB2, HIGH);
    }  
    else
    {
        digitalWrite(ard_pi_BB2, LOW);
    }

    //servo movement loops
    if (pos1_current<pos1_target) //SERVO 1
    {
        pos1_current=pos1_current+1;
        servo1.write(pos1_current);     
        delayMicroseconds(slowness); 
    }
    if (pos1_current>pos1_target)
    {
        pos1_current=pos1_current-1;
        servo1.write(pos1_current);     
        delayMicroseconds(slowness); 
    }

    if (pos2_current<pos2_target) //SERVO 2
    {
        pos2_current=pos2_current+1;
        servo2.write(pos2_current);     
        delayMicroseconds(slowness); 
    }
    if (pos2_current>pos2_target)
    {
        pos2_current=pos2_current-1;
        servo2.write(pos2_current);     
        delayMicroseconds(slowness); 
    }

    //target commands
    //SERVO 1
    if ((digitalRead(pi_ard_door1) == HIGH)) 
    {
        pos1_target=OPEN_DOOR1;
    }
    else 
    {
        pos1_target=CLOSE_DOOR1;
    }
    //SERVO 2
    if ((digitalRead(pi_ard_door2) == HIGH)) 
    {
        pos2_target=OPEN_DOOR2;
    }
    else 
    {
        pos2_target=CLOSE_DOOR2;
    }
    
}//void loop end
