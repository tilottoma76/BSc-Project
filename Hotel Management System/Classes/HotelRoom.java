package Classes;

import java.lang.*;
import Interface.HotelRoomOperation;

public abstract class HotelRoom implements HotelRoomOperation
{	
	protected int roomNumber;
    protected String roomHolderName;
	protected double prize;
	protected int days;

	public HotelRoom()
	{
		
	}
	public HotelRoom(String roomHolderName, double prize,int roomNumber,int days)
	{
		
		this.roomNumber = roomNumber;
		this.roomHolderName =roomHolderName;
		this.prize = prize;
		this.days = days;
	}

	public void setRoomNumber(int roomNumber){ this.roomNumber = roomNumber; }
	public void setDays(int days){ this.days = days; }
	public void setRoomHolderName(String roomHolderName){	this.roomHolderName = roomHolderName;	}
	public void setprize(double prize){	this.prize = prize; }
	
	
	public String getRoomHolderName(){	return roomHolderName; }
	public double getprize(){ 
         prize = prize;
		return prize;	}
	public int getRoomNumber(){	return roomNumber; }
	public int getDays(){	
		return days; }
	public abstract void showDetails();
	
	public void  discountRate(int discount)
	{
		
		
			prize = (prize*days) - discount;
	
                    System.out.println("Totl prize after discount "+prize);
	
      

	}

}