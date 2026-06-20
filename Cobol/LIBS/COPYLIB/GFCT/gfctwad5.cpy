      *------------------------ INC GFCTWAD5 -------------------------*
      *                                                               *
      *                EXTRATO ANUAL DE TARIFAS BRADESCO              *
      *                                                               *
      *                   ARQUIVO CADASTRO DE CLIENTES                *
      *                     IDENTIFICACAO DE CLIENTE                  *
      *                                                               *
ST2507*                                      (VB) TAMANHO=0147 BYTES  *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                  *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*

       01  REG-CADASTRO.
           03  D5-CAD-CHAVE.
               05  D5-CAD-CEP-CHAVE        PIC  9(09).
               05  D5-CAD-AGENCIA          PIC  9(05).
               05  D5-CAD-CONTA            PIC  9(07).
               05  D5-CAD-ASSUNTO          PIC  9(04).
               05  D5-CAD-SEGMENTO         PIC  9(03).
               05  D5-CAD-SEQUENCIA        PIC  9(03).
               05  D5-CAD-SEGMENTACAO      PIC  X(01).
           03  D5-CAD-DADOS.
               05  D5-CAD-NOME             PIC  X(40).
               05  D5-CAD-ENDERECO         PIC  X(40).
               05  D5-CAD-DDD-FONE         PIC  9(05)     COMP-3.
               05  D5-CAD-FONE             PIC  9(07)     COMP-3.
               05  D5-CAD-DDD-FAX          PIC  9(05)     COMP-3.
               05  D5-CAD-FAX              PIC  9(07)     COMP-3.
ST2507*        05  D5-CAD-NRO-CGC-CPF      PIC  9(09)     COMP-3.
ST2507*        05  D5-CAD-FIL-CGC-CPF      PIC  9(05)     COMP-3.
ST2507         05  D5-CAD-NRO-CGC-CPF      PIC  X(09).
ST2507         05  D5-CAD-FIL-CGC-CPF      PIC  X(04).
               05  D5-CAD-CTR-CGC-CPF      PIC  9(02).
               05  D5-CAD-CEP-NRO          PIC  9(05)     COMP-3.
               05  D5-CAD-CEP-COMPL        PIC  9(03)     COMP-3.
               05  D5-CAD-TIPO             PIC  X(01).
