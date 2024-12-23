package Classes;

import java.lang.*;

public class VipRoom extends HotelRoom
{
	
    private String roomType;
	
	public VipRoom()
	{
		super();
		
	}
	public VipRoom( String  roomHolderName, double prize,int roomNumber, int days,String roomType)
	{
		super(roomHolderName, prize, roomNumber,days);
		this.roomType = roomType;
		
		
	}
	public void setroomType(String roomType){
		this.roomType= roomType;
	}
	public String getroomType(){
		return roomType;
	}
	

	public void showDetails()
	{
		System.out.println("Room Number : " + roomNumber);
		System.out.println("Room Holder Name : " + roomHolderName);
		System.out.println("Room Type: "+ roomType);
		System.out.println("Days Stay "+ days);
		System.out.println("Prize : " +  prize);

	
	}
	
}