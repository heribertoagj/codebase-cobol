      ******************************************************************
      * NOME BOOK : DCOMW25O - BOOK DE INTERFACE COM SERVICO DCOMW25O  *
      * DATA      : 18/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 591 BYTES                                          *
      ******************************************************************
       05 DCOMW25O-HEADER.
         10 DCOMW25O-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW25O'.
         10 DCOMW25O-TAM-LAYOUT        PIC 9(05)   VALUE 591.
       05 DCOMW25O-REGISTRO.
         10 DCOMW25O-BLOCO-ENTRADA.
           15 DCOMW25O-E-CCONVE-GRAL-DESC          PIC 9(09).
           15 DCOMW25O-E-CVRSAO-CONVE-GRAL         PIC 9(03).
           15 DCOMW25O-E-CTPO-AGPTO-ROTRO          PIC 9(03).
         10 DCOMW25O-BLOCO-SAIDA.
           15 DCOMW25O-S-CCONVE-GRAL-DESC          PIC 9(09).
           15 DCOMW25O-S-CVRSAO-CONVE-GRAL         PIC 9(03).
           15 DCOMW25O-S-CTPO-AGPTO-ROTRO          PIC 9(03).
           15 DCOMW25O-S-CINDCD-OBRIG-AGPTO        PIC X(01).
           15 DCOMW25O-S-CINDCD-SIT-AGPTO          PIC X(01).
           15 DCOMW25O-S-CINDCD-CONCL-ROTRO        PIC X(01).
           15 DCOMW25O-S-DVALDC-ROTRO-ELMTO        PIC X(10).
           15 DCOMW25O-S-HULT-ATULZ                PIC X(26).
           15 DCOMW25O-S-CFUNC-BDSCO               PIC 9(09).
           15 DCOMW25O-S-CTERM                     PIC X(08).
