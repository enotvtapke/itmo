package info.kgeorgiy.ja.stupnikov.bank;

import java.io.Serializable;
import java.rmi.RemoteException;

public class LocalAccount extends AbstractAccount implements Serializable {

    public LocalAccount(String id) {
        super(id);
    }

    public LocalAccount(Account account) throws RemoteException {
        super(account.getId());
    }
}
