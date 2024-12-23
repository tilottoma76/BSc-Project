package Classes;

import java.lang.*;

public class AcRoom extends HotelRoom
{
	
    private String roomTypeA;
	
	public AcRoom()
	{
		super();
		
	}
	public AcRoom( String  roomHolderName, double prize,int roomNumber, int days,String roomTypeA)
	{
		super(roomHolderName, prize, roomNumber,days);
		this.roomTypeA = roomTypeA;
		
		
	}
	public void setroomTypeA(String roomType){
		this.roomTypeA= roomTypeA;
	}
	public String getroomTypeA(){
		return roomTypeA;
	}

      


	public void showDetails()
	{
		System.out.println("Room Number : " + roomNumber);
		System.out.println("Room Holder Name : " + roomHolderName);
		System.out.println("Room Type: "+ roomTypeA);
		System.out.println("Days Stay "+ days);
		System.out.println("Prize : " +  prize);

	
	}
	
}