      *----------------------------------------------------------------*
      *  I#BVVE71 - LIQUIDACAO FINANCEIRA RETIDOS                      *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAA-MM-DDTHH:MM:SS:MMMMMM                 *
      *  TAMANHO LAYOUT:                                               *
      *----------------------------------------------------------------*

       01 BVVE71-REGISTRO.
          05 BVVE71-PERFIL                   PIC  9(08).
          05 BVVE71-NOME-CREDC               PIC  X(27).
          05 BVVE71-DETALHE.
          07 BVVE71-NUM-LIQUID               PIC  X(21).
          07 BVVE71-INFO-RETORNO             PIC  X(10).
          07 BVVE71-CNPJ-CPF                 PIC  9(09) .
          07 BVVE71-FILIAL                   PIC  9(04).
          07 BVVE71-CTRL                     PIC  9(02).
          07 BVVE71-AGENCIA                  PIC  9(05).
          07 BVVE71-CONTA                    PIC  9(07).
          07 BVVE71-DATA                     PIC  9(08).
          07 BVVE71-VALOR                    PIC  9(13)V99.
          07 BVVE71-BANDEIRA                 PIC  9(03).
          07 BVVE71-COD-CRED-PROD            PIC  9(04).
          07 BVVE71-VALOR-RETD               PIC  9(13)V99.
          07 BVVE71-CONSIS                   PIC  X(01).
          07 BVVE71-TIP-RETEN                PIC  X(01).
          07 BVVE71-FLAG-RET                 PIC  X(01).
          07 BVVE71-AGENDAMENTO              PIC  X(01).
          07 BVVE71-CONSISTIDO               PIC  X(01).
          07 BVVE71-TIP-CONTA                PIC  X(02).
          07 FILLER                          PIC  X(08).
