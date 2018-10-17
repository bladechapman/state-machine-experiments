
/* type stateMachineType ('stateType, 'eventType, 'commandType) = {
    currentState: 'stateType,
    handleEvent: 'eventType => ('commandType, stateMachineType ('stateType, 'eventType, 'commandType))
}

type turnstyleState = Locked(int) | Unlocked;
type turnstyleEvent = InsertCoin(int) | AdmitPerson | MachineDidFail | MachineRepairComplete;
type turnstyleCommand = SoundAlarm | CloseDoors | OpenDoors;
type turnstyleType = stateMachineType(turnstyleState, turnstyleEvent, turnstyleCommand); */


module type StateMachine {
    module States { type stateType; }
    module Events { type eventType; }
    module Commands { type commandType }

    open States; open Commands; open Events;
    let handleEvent: (stateType, eventType) => (stateType, commandType)
};

module TurnstyleStateMachine: StateMachine {
    module States { type stateType = Locked(int) | Unlocked | Broken (stateType) };
    module Events { type eventType = InsertCoin(int) | AdmitPerson | MachineDidFail | MachineRepairComplete };
    module Commands { type commandType = SoundAlarm | CloseDoors | OpenDoors | Noop };

    let farePrice = 50;

    let handleEvent (s, e) = {
        open States; open Events; open Commands;
        switch (s, e) {
        | (Locked(credit), InsertCoin(value)) =>
            let newCredit = credit + value;
            let farePriceExceeded = newCredit >= farePrice;
            switch (farePriceExceeded) {
            | true => (Unlocked, OpenDoors)
            | false => (Locked(newCredit), Noop)
            }
        | (Locked(_), AdmitPerson) => (s, SoundAlarm)
        | (Locked(_), MachineDidFail) => (Broken(s), Noop)
        | (Unlocked, AdmitPerson) => (Locked(0), CloseDoors)
        | (Unlocked, MachineDidFail) => (Broken(s), Noop)
        | (Broken(s), MachineRepairComplete) => (s, Noop)
        | (_, _) => (s, Noop)
        }
    }
};

