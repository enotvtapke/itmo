package rmi;

abstract public class AbstractAccount implements Account {
    private String id;
    private int amount;

    public AbstractAccount(final String id) {
        this.id = id;
        amount = 0;
    }

    public AbstractAccount() {
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public synchronized int getAmount() {
        System.out.println("Getting amount of money for account " + id);
        return amount;
    }

    @Override
    public synchronized void setAmount(final int amount) {
        System.out.println("Setting amount of money for account " + id);
        this.amount = amount;
    }
}
