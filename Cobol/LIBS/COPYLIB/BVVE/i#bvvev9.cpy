      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *                                                                *
      *  I#BVVEV9 - RETORNO DO MODULO COSA2080 (CREDITO) + SEQUENCIAL  *
      *  TAMANHO: 250                                                  *
      *----------------------------------------------------------------*

       01  BVVEV9-REGISTRO.
           03  BVVEV9-INPUT.
               05  BVVEV9-BCO-AGE-CLIENTE.
                   07  BVVEV9-BANCO            PIC  9(03)     COMP-3.
                   07  BVVEV9-AGENCIA          PIC  9(05)     COMP-3.
                   07  BVVEV9-NUM-CONTA        PIC  9(13)     COMP-3.
                   07  BVVEV9-DIG-CONTA        PIC  X(02).
               05  BVVEV9-RAZAO.
                   07  BVVEV9-GRUPO            PIC  9(03)     COMP-3.
                   07  BVVEV9-SUBGRUPO         PIC  9(03)     COMP-3.
               05  BVVEV9-CCUSTO               PIC  X(04).
               05  BVVEV9-COD-LCTO             PIC  9(05)     COMP-3.
               05  BVVEV9-NUM-DOCTO            PIC  9(15)     COMP-3.
               05  BVVEV9-VLR-TOTAL            PIC S9(13)V99  COMP-3.
               05  BVVEV9-TP-CREDITO           PIC  X(01).
               05  BVVEV9-TERMINAL-ORIGEM      PIC  X(08).
               05  BVVEV9-LIN-HIST-COMPL       PIC  X(32).
           03  BVVEV9-OUTPUT.
               05  BVVEV9-SIT-CONTA            PIC  9(01).
               05  BVVEV9-VLR-TOT-CREDITO      PIC S9(13)V99  COMP-3.
               05  BVVEV9-SALDO-ANTERIOR.
                   07  BVVEV9-SALDANT-LIVRE    PIC S9(13)V99  COMP-3.
                   07  BVVEV9-SALDANT-RESDISP  PIC S9(13)V99  COMP-3.
                   07  BVVEV9-SALDANT-TOTAL    PIC S9(13)V99  COMP-3.
               05  BVVEV9-HORA                 PIC  9(06).
               05  BVVEV9-COD-RETORNO          PIC  9(07)     COMP-3.
               05  BVVEV9-MENSAGEM             PIC  X(80).
           03  BVVEV9-SEQUENCIAL               PIC  9(09)     COMP-3.
           03  BVVEV9-RESERVA                  PIC  X(40).
