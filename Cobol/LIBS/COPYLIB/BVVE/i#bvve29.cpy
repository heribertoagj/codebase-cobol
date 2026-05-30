      *----------------------------------------------------------------*
      * BOOK I#BVVE29     ARQUIVO DE LOG DE MARCA/DESMARCA RECEBIVEIS  *
      * TAMANHO : 066                                                  *
      * AGOSTO/2018: EXPANSAO DO CAMPO PRODUTO PARA 5DIGITOS           *
      *----------------------------------------------------------------*
      *
       01  REG-BVVEB029.
           05 BVVEB029-AGENCIA      PIC S9(05)V     USAGE COMP-3.
           05 BVVEB029-CONTA        PIC S9(07)V     USAGE COMP-3.
           05 BVVEB029-PRODUTO      PIC S9(05)V     USAGE COMP-3.
           05 BVVEB029-CCTRO-CUSTO  PIC  X(04).
           05 BVVEB029-DMOVTO       PIC  X(10).
           05 BVVEB029-COD-OPERACAO PIC S9(13)V     USAGE COMP-3.
           05 BVVEB029-CTPO-OPER    PIC  X(01).
           05 BVVEB029-HATULZ       PIC  X(26).
           05 BVVEB029-VALOR        PIC S9(13)V9(2) USAGE COMP-3.
