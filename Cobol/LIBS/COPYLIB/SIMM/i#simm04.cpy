      *----------------------------------------------------------------*
      *    INC 'I#SIMM04'       - AREA DE COMUNICACAO DE MENSAGENS     *
      *    TAMANHO              - 51   BYTES                           *
      *                                                                *
      *    ANALISTA RESPONSAVEL - INES/MARCILENE (PRIME)               *
      *    ATUALIZACAO          - 03/10/2001                           *
      *    DESCRICAO            - BLOCO DA MENSAGERIA  - BCMSG         *
      *----------------------------------------------------------------*
      *
       01  SIMM04-REGISTRO.
           03  SIMM04-BLOCO-BCMSG.
               05  SIMM04-CEMISR-MSGEM        PIC  X(0001).
               05  SIMM04-CSIST-PGTO-EMISR    PIC  X(0008).
               05  SIMM04-CDSTNA-MSGEM        PIC  X(0001).
               05  SIMM04-CSIST-PGTO-DSTNA    PIC  X(0008).
               05  SIMM04-GCNRO-SEQ-BACEN.
                   07  SIMM04-CNRO-SEQ-BACEN  PIC  9(0009).
               05  SIMM04-CINDCD-CONTINUA   PIC  X(0001).
               05  SIMM04-CNRO-UNIC-OPER    PIC  X(0023).
