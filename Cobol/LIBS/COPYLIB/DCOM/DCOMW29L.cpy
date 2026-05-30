      ******************************************************************
      * NOME BOOK : DCOMW29L - BOOK DE INTERFACE                       *
      * DESCRICAO : CONSULTA CHEQUES DA OPERACAO                       *
      * DATA      : 17/11/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 05778 BYTES                                        *
      ******************************************************************
       05 DCOMW29L-HEADER.
         10 DCOMW29L-COD-LAYOUT              PIC X(08) VALUE 'DCOMW29L'.
         10 DCOMW29L-TAM-LAYOUT                  PIC 9(05) VALUE 05778.
       05 DCOMW29L-REGISTRO.
         10 DCOMW29L-BLOCO-ENTRADA.
           15 DCOMW29L-E-DANO-OPER               PIC 9(04).
           15 DCOMW29L-E-NSEQ-OPER               PIC 9(09).
         10 DCOMW29L-BLOCO-SAIDA.
           15 DCOMW29L-S-QTD-CHEQUES             PIC 9(02).
           15 DCOMW29L-S-SAI-OCORRENCIAS.
             20  DCOMW29L-S-OCORRENCIAS OCCURS 50 TIMES.
               25 DCOMW29L-S-CBCO                PIC 9(03).
               25 DCOMW29L-S-CAG-BCRIA           PIC 9(05).
               25 DCOMW29L-S-CCTA-CORR-BCRIA     PIC 9(13).
               25 DCOMW29L-S-CCHEQ               PIC 9(07).
               25 DCOMW29L-S-CCNPJ-CPF           PIC 9(09).
               25 DCOMW29L-S-CCNPJ-FILIAL        PIC 9(04).
               25 DCOMW29L-S-CCTRL-CNPJ-CPF      PIC 9(02).
               25 DCOMW29L-S-DVCTO-CHEQ-CTDIA    PIC X(10).
               25 DCOMW29L-S-VCHEQC              PIC 9(13)V9(2).
               25 DCOMW29L-S-VJURO-CHEQC         PIC 9(9)V9(6).
               25 DCOMW29L-S-VIOF-CHEQC          PIC 9(9)V9(6).
               25 DCOMW29L-S-VLIQUIDO-CHEQC      PIC 9(15)V99.
      *---------------------------------------------------------------*
      *    F I M     D E    B O O K                                   *
      *---------------------------------------------------------------*
