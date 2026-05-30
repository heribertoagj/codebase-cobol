      *----------------------------------------------------------------*
      *  I#BVVE77 - LIQUIDACAO FINANCEIRA RETIDOS                      *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAA-MM-DDTHH:MM:SS:MMMMMM                 *
      *  TAMANHO LAYOUT:                                               *
      *----------------------------------------------------------------*

       01 BVVE77-REGISTRO.
          05 BVVE77-REGISTRO-RED.
             07 BVVE77-PERFIL                   PIC  9(08).
             07 BVVE77-NOME-CREDC               PIC  X(27).
             07 BVVE77-NUM-LIQUID               PIC  X(21).
             07 BVVE77-INFO-RETORNO             PIC  X(10).
             07 BVVE77-CNPJ-CPF                 PIC  9(09) .
             07 BVVE77-FILIAL                   PIC  9(04).
             07 BVVE77-CTRL                     PIC  9(02).
             07 BVVE77-AGENCIA                  PIC  9(05).
             07 BVVE77-CONTA                    PIC  9(07).
             07 BVVE77-DATA                     PIC  9(08).
             07 BVVE77-VALOR-ORI                PIC  9(13)V99.
             07 BVVE77-BANDEIRA                 PIC  9(03).
             07 BVVE77-COD-CRED-PROD            PIC  9(04).
             07 BVVE77-VALOR-RETD               PIC  9(13)V99.
             07 BVVE77-CONSIS                   PIC  X(01).
             07 BVVE77-TIP-RETEN                PIC  X(01).
             07 BVVE77-FLAG-RET                 PIC  X(01).
             07 BVVE77-AGENDAMENTO              PIC  X(01).
             07 BVVE77-CONSISTIDO               PIC  X(01).
             07 BVVE77-TP-CONTA                 PIC  X(02).
             07 FILLER                          PIC  X(08).
          05 BVVE77-REGISTRO-EXP.
             07 BVVE77-VALOR-MPAG               PIC  9(13)V99.
             07 BVVE77-CRED-DEB                 PIC  X(001).
             07 BVVE77-TIP-OPER                 PIC  X(001).
             07 FILLER                          PIC  X(130).
