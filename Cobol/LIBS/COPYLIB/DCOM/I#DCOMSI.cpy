      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSI                                      *
      * TAMANHO : 0075 BYTES                                           *
      * ARQUIVO : CONVENIO CONTROLE FORNECEDOR                         *
      * INCLUDE DA TABELA DCOMB097 - TCONVE_CTRL_FORNC                 *
      ******************************************************************
       01  SI-97-REGISTRO.
  1  5     05 SI-97-CCONVE-CLI-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     05 SI-97-CVRSAO-CONVE-CLI     PIC S9(3)V USAGE COMP-3.
  8  5     05 SI-97-CCNPJ-CPF-FORNC      PIC S9(9)V USAGE COMP-3.
 13  3     05 SI-97-CFLIAL-CNPJ-FORNC    PIC S9(5)V USAGE COMP-3.
 16 10     05 SI-97-DVCTO-PGTO-FORNC     PIC X(10).
 26  2     05 SI-97-CCTRL-CNPJ-FORNC     PIC S9(2)V USAGE COMP-3.
 28  9     05 SI-97-VUTLZD-VCTO-FORNC    PIC S9(15)V9(2) USAGE COMP-3.
 37 26     05 SI-97-HULT-ATULZ           PIC X(26).
 63  5     05 SI-97-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
 68  8     05 SI-97-CTERM                PIC X(8).
