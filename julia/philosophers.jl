function createfork(_)
    ReentrantLock()
end

function take(fork)
    lock(fork)
end

function drop(fork)
    unlock(fork)
end

function eat(name)
    @printf "%s is eating...\n" name
    busy()
end

function think(name)
    busy()
end

function busy()
    sleep(0.5 + 4.5 * rand())
end

function philosopher(name, leftfork, rightfork)
    while true
        take(leftfork)
        take(rightfork)
        eat(name)
        drop(rightfork)
        drop(leftfork)
        think(name)
    end
end

philosophers = [:plato, :confucius, :socrates, :voltaire, :descartes]
forks = map(createfork, 1:length(philosophers))

srand()
for (name, leftfork, rightfork) in zip(philosophers, forks, circshift(forks, 1))
    @schedule philosopher(name, leftfork, rightfork)
end
