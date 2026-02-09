import random
import sys

import pygame
from pygame import Vector2

WHITE = (255, 255, 255)
RED = (200, 0, 0)
BLUE1 = (0, 0, 255)
BLUE2 = (0, 100, 255)
BLACK = (0, 0, 0)

cell_size = 20
num_cells = 40

# pygame definitions
pygame.init()
screen = pygame.display.set_mode((cell_size * num_cells, cell_size * num_cells))
pygame.display.set_caption("Snake")
clock = pygame.time.Clock()
my_font = pygame.font.Font(None, 50)


class Direction:
    RIGHT: int = 1
    LEFT: int = 2
    UP: int = 3
    DOWN: int = 4


class Food:
    def __init__(self):
        self.position = Vector2(random.randint(1, num_cells - 1), random.randint(1, num_cells - 1))

    def display(self):
        food_rect = pygame.Rect(self.position.x * cell_size, self.position.y * cell_size, cell_size, cell_size)
        pygame.draw.rect(screen, RED, food_rect)

    def rand_spawn(self, snake):
        if snake.body:
            free = [[x, y] for x in range(num_cells) for y in range(num_cells)]
            for pos in snake.body:
                free.remove([pos.x, pos.y])

            idx = random.randint(0, len(free) - 1)
            self.position = Vector2(free[idx][0], free[idx][1])


class Snake:
    def __init__(self, direction=Direction.RIGHT):
        self.body = [Vector2(num_cells // 2, num_cells // 2)]
        self.direction = direction
        self.add_segment = False
        self.last_pos = Vector2(num_cells // 2, num_cells // 2)

    def display(self):
        for segment in self.body:
            snake_rect = pygame.Rect(segment.x * cell_size, segment.y * cell_size, cell_size, cell_size)
            pygame.draw.rect(screen, BLUE2, snake_rect)

    def move_snake(self):
        x = self.body[0].x
        y = self.body[0].y
        if self.direction == Direction.RIGHT:
            x += 1
        elif self.direction == Direction.LEFT:
            x -= 1
        elif self.direction == Direction.UP:
            y -= 1
        elif self.direction == Direction.DOWN:
            y += 1
        self.body.insert(0, Vector2(x, y))

        self.last_pos, self.body = self.body[-1], self.body[:-1]

    def grow_snake(self):
        self.body.insert(len(self.body), self.last_pos)
        self.add_segment = False


class Game:
    def __init__(self):
        self.snake = Snake()
        self.food = Food()
        self.end_game = False

    def check_food(self):
        p1 = self.snake.body[0]
        p2 = self.food.position

        if p1 == p2:
            self.snake.grow_snake()
            self.food.rand_spawn(self.snake)

    def run_game(self):
        self.check_food()
        self.food.display()
        self.snake.display()
        self.snake.move_snake()
        self.check_collision()

    def check_collision(self):
        head_x, head_y = self.snake.body[0].x, self.snake.body[0].y

        if head_x not in range(num_cells) or head_y not in range(num_cells):
            self.end_game = True
        for x, y in self.snake.body[1:]:
            if x == head_x and y == head_y:
                self.end_game = True


game = Game()

# game loop
while True:
    screen.fill(BLACK)

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            sys.exit()

        direction_changed = False

        if event.type == pygame.KEYDOWN and not direction_changed:
            if event.key == pygame.K_LEFT and game.snake.direction != Direction.RIGHT:
                game.snake.direction = Direction.LEFT
            elif event.key == pygame.K_RIGHT and game.snake.direction != Direction.LEFT:
                game.snake.direction = Direction.RIGHT
            elif event.key == pygame.K_UP and game.snake.direction != Direction.DOWN:
                game.snake.direction = Direction.UP
            elif event.key == pygame.K_DOWN and game.snake.direction != Direction.UP:
                game.snake.direction = Direction.DOWN
            direction_changed = True

    game.run_game()

    score_surf = my_font.render(f"Score: {len(game.snake.body) - 1}", True, WHITE)
    screen.blit(score_surf, (10, 10))

    pygame.display.update()
    clock.tick(10)

    if game.end_game:
        screen.fill(BLACK)
        game_over_text = my_font.render(f"Game Over! Score: {len(game.snake.body)}", True, WHITE)
        text_rect = game_over_text.get_rect(center=(cell_size * num_cells // 2, cell_size * num_cells // 2))
        screen.blit(game_over_text, text_rect)
        pygame.display.update()
        pygame.time.wait(2000)
        game = Game()
