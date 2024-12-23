package Interface;
import java.lang.*;
import Classes.Employee;

public interface HotelEmployeeOperation
{
	boolean insertEmployee(Employee e);
	boolean removeEmployee(Employee e);
	Employee searchEmployee(String empId);
	void showAllEmployees();
}