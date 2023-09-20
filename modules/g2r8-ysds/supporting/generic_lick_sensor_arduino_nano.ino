#include <CapacitiveSensor.h>
CapacitiveSensor   cs_10_11 = CapacitiveSensor(10,11);// 10Mohm resistor between pins 10 & 11, pin 11 is sensor pin
//Capacitive sensor
int c_thresh;//look at serial monitor during testing to find a value
int var;
int c;
int threshold_increment;
//outputs to Pi
const int ard_pi_lick = 13;//output TTL, pin 13 also turns on the built-in LED

void setup() 
{
    cs_10_11.set_CS_AutocaL_Millis(0xFFFFFFFF);// turn off autocalibrate on channel 1 - just as an example
    Serial.begin(9600);//setup serial
    pinMode(ard_pi_lick, OUTPUT);
    threshold_increment = 1000;//change threshold here if needed
    //collect lick sensor bsl
    delay(500);
    var = 0;
    c_thresh=0;
    while (var < 5) 
    {
      c_thresh =  c_thresh+cs_10_11.capacitiveSensor(1000);//change sensitivity here if needed 1/2
      var++;
    }
    c_thresh=(c_thresh/5)+threshold_increment; 
    Serial.print("cs_threshold   ");//show value for trouble shooting if serial monitor is on
    Serial.println(c_thresh); 
}

void loop() 
{
    long c = cs_10_11.capacitiveSensor(1000);//change sensitivity here if needed 2/2
    var = 0;
    Serial.print("\t");// tab character for debug windown spacing
    Serial.println(c);
    if (c<c_thresh) //capacitive sensor
    {
        digitalWrite(ard_pi_lick, LOW);
    }  
    else
    {
        digitalWrite(ard_pi_lick, HIGH);
    } 
}
