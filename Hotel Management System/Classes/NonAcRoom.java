package Classes;

import java.lang.*;

public class NonAcRoom extends HotelRoom
{
		
private String roomTypeN;
	
	public NonAcRoom()
	{
		super();
		
	}
	public NonAcRoom( String  roomHolderName, double prize,int roomNumber,int days,String roomTypeN)
	{
		super( roomHolderName, prize,roomNumber,days);
	    this.roomTypeN = roomTypeN;
		
		
	}
	
	public void setRoomTypeN(String roomTypeN){
		this.roomTypeN= roomTypeN;
	}
	public String getRoomTypeN(){
		return roomTypeN;
	}

	
	public void showDetails()
	{
		System.out.println("Room Number : " +roomNumber);
		System.out.println("Room Holder Name : " + roomHolderName);
		System.out.println("Room Type: "+ roomTypeN);
		System.out.println("Days Stay "+ days);
		System.out.println("Prize : " +  prize);

	
	}
	
}