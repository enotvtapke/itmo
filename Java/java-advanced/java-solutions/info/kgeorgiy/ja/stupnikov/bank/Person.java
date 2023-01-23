package info.kgeorgiy.ja.stupnikov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Person extends Remote {
    /** Return person first name */
    String getFirstName() throws RemoteException;

    /** Return person last name */
    String getLastName() throws RemoteException;

    /** Return person passport id */
    String getPassportId() throws RemoteException;
}
