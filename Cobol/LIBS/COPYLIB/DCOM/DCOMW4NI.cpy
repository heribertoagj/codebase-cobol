      ******************************************************************
      * NOME BOOK : DCOMW4NI - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DESCRICAO : CONSULTA INFORMACOES DE BANCO E AGENCIA (UORG)     *
      * DATA      : 27/09/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 141 BYTES                                          *
      ******************************************************************
       05 DCOMW4NI-HEADER.
         10 DCOMW4NI-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW4NI'.
         10 DCOMW4NI-TAM-LAYOUT        PIC 9(05)   VALUE 141.
       05 DCOMW4NI-REGISTRO.
         10 DCOMW4NI-BLOCO-ENTRADA.
           15 DCOMW4NI-E-CODIGO-BANCO         PIC  9(03).
           15 DCOMW4NI-E-CODIGO-AGENCIA       PIC  9(05).
         10 DCOMW4NI-BLOCO-SAIDA.
           15 DCOMW4NI-S-NOME-BANCO           PIC  X(70).
           15 DCOMW4NI-S-NOME-AGENCIA         PIC  X(50).
