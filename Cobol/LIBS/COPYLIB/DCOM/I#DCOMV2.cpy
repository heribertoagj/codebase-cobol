      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMV2 *
      * CRIACAO   : 02/2009                                   *
      * DESCRICAO : TABELA DE MODULOS DE NEGOCIOS             *
      *                                                       *
      * APLICACAO : MODULO - DCOM0003 - OPERACOES;            *
      *                                                       *
      *                                                       *
      * TAMANHO   : LRECL -                                   *
      *                                                       *
      *********************************************************

       01  V2-MAPA-TRANS-D.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100001DCOM5800'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100002DCOM5801'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100003DCOM5802'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100004DCOMXXXX'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100005DCOM5804'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100006DCOM5805'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100007DCOM5806'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100008DCOM5807'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100009DCOM5803'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100010DCOMXXXX'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100011DCOMXXXX'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100012DCOMXXXX'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100013DCOMXXXX'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100014DCOMXXXX'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100015DCOMXXXX'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100016DCOMXXXX'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100017DCOMXXXX'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100018DCOMXXXX'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100019DCOMXXXX'.
           03 FILLER    PIC X(0024) VALUE 'DCOM001000100020DCOMXXXX'.
       01  V2-MAPA-TRANS-R REDEFINES V2-MAPA-TRANS-D.
           03 V2-MAPA-TRANS OCCURS 020 TIMES INDEXED BY INDTAB.
              05 V2-CHAVE.
                 07 V2-ROTEADOR   PIC X(0008).
                 07 V2-EVENTO     PIC X(0004).
                 07 V2-FUNCAO     PIC X(0004).
              05 V2-MODULO        PIC X(0008).
