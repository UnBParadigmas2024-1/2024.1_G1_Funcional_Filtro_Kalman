import ChartModule

-- Na main a funcao generateChart recebe 2 vetores como especificado

main :: IO ()
main = do
    let z1 = [1,2,3,4,5,6,7,8,9,10]
        z2 = [10.0511, 9.9858, 10.1017, 9.8966, 10.0792, 9.9726, 10.0042 , 9.8847, 10.0458 , 9.9352]
    generateChart z1 z2

