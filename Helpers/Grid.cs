namespace Helpers;

/// <summary>
/// Copied from last year, mainly used for diagnostics
/// </summary>
public static class Grid
{
    public static int[,] ParseNumberGrid(string input)
        => Grid<int>.ParseGrid(input, CharToInt);

    private static int CharToInt(char c)
        => c switch
        {
            '0' => 0,
            '1' => 1,
            '2' => 2,
            '3' => 3,
            '4' => 4,
            '5' => 5,
            '6' => 6,
            '7' => 7,
            '8' => 8,
            '9' => 9,
        };
}

public static class Grid<T> where T : notnull
{
    public static T[,] ParseGrid(string input, Func<char, T> inputConverter)
    {
        var rows = input.Split(Environment.NewLine);

        var width = rows[0].Length;        // Assume they're all equal
        var height = rows.Count();

        T[,] result = new T[width, height];

        for (int r = 0; r < height; r++)
        {
            var cols = rows[r].ToCharArray();
            for (int c = 0; c < width; c++)
            {
                result[c, r] = inputConverter(cols[c]);
            }
        }

        return result;
    }

    public static void PrintArray(T[,] input,
        Func<int, int, T, bool>? highlight = null,
        Func<T, string>? conversion = null)
    {
        highlight ??= (x, y, i) => false;
        conversion ??= i => i.ToString();       // TODO I said T is notnull, why does it warn here?

        int width = input.GetLength(0);
        int height = input.GetLength(1);
        for (int r = 0; r < height; r++)
        {
            for (int c = 0; c < width; c++)
            {
                var me = input[c, r];
                var mePresentable = conversion(me);
                Console.BackgroundColor = highlight(c, r, me) switch
                {
                    false => ConsoleColor.Black,
                    true => ConsoleColor.Red,
                };
                Console.ForegroundColor = highlight(c, r, me) switch
                {
                    false => ConsoleColor.Gray,
                    true => ConsoleColor.White,
                };
                Console.Write(mePresentable);
                Console.BackgroundColor = ConsoleColor.Black;
                Console.ForegroundColor = ConsoleColor.Gray;
            }
            Console.WriteLine();
        }

        for (int i = 0; i < width + 15; i++) Console.Write("-");
        Console.WriteLine();
    }

    public static void ManipulateGridCells(T[,] input, Func<T, T> manipulation)
    {
        Action<T[,], int, int> thunkedManipulation
            = (T[,] i, int x, int y) => i[x, y] = manipulation(i[x, y]);

        ManipulateGridCells(input, thunkedManipulation);
    }

    public static void ManipulateGridCells(T[,] input, Action<T[,], int, int> manipulation)
    {
        int width = input.GetLength(0);
        int height = input.GetLength(1);

        for (int r = 0; r < height; r++)
        {
            for (int c = 0; c < width; c++)
            {
                manipulation(input, c, r);
            }
        }
    }
}