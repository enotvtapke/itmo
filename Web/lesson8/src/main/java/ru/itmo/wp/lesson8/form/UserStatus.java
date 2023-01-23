package ru.itmo.wp.lesson8.form;

import javax.validation.constraints.NotNull;

@SuppressWarnings("unused")
public class UserStatus {
    @NotNull
    private String id;

    @NotNull
    private String disabled;

    public Long getId() {
        try {
            return Long.parseLong(id);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    public void setId(String id) {
        this.id = id;
    }

    public Boolean isDisabled() {
        if (disabled.equals("1")) {
            return true;
        } else if(disabled.equals("0")) {
            return false;
        } else {
            return null;
        }
    }

    public void setDisabled(String disabled) {
        this.disabled = disabled;
    }
}
