package Interface;

import java.lang.*;
import Classes.HotelRoom;

public interface CustomerOperation
{
	 boolean insertRoom(HotelRoom a);
	boolean removeRoom(HotelRoom a);
	HotelRoom searchRoom(int roomNumber);
	void showAllRooms();
}