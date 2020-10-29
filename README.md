# tic-tac-toe

Сервер-клиент реализации игры в крестики нолики. Сервер поддерживает множество соединений.

Алгоритм компьютера делает случайный ход в одну из свободных ячеек.

Поддерживаемый размер поля от 2.

Игра завершается, когда есть n ноликов или крестиков в строке, в столбце или на диагонали, 
где n размер доски.


сборка проекта
stack build

запуск сервера
stack exec tic-tac-toe-server

запуск клиента 
stack exec tic-tac-toe-client
 
(далее вручную ввести адрес 127.0.0.1 и порт 4000)