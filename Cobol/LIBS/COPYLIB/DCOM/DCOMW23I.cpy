      ******************************************************************
      * NOME BOOK : DCOMW23I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DESCRICAO : CONSULTA INFORMACAO AGENCIA                        *
      * DATA      : 27/09/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 116 BYTES                                          *
      ******************************************************************
       05 DCOMW23I-HEADER.
         10 DCOMW23I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW23I'.
         10 DCOMW23I-TAM-LAYOUT        PIC 9(05)   VALUE 116.
       05 DCOMW23I-REGISTRO.
         10 DCOMW23I-BLOCO-ENTRADA.
           15 DCOMW23I-E-CAG-BCRIA                 PIC 9(05).
         10 DCOMW23I-BLOCO-SAIDA.
           15 DCOMW23I-S-NOME                      PIC X(50).
           15 DCOMW23I-S-FONE                      PIC 9(08).
           15 DCOMW23I-S-EMAIL                     PIC X(40).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
