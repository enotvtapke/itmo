package rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Person extends Remote {
    String getFirstName() throws RemoteException;
    String getLastName() throws RemoteException;
    String getPassportId() throws RemoteException;
}
