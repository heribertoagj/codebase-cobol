      ******************************************************************
      * NOME BOOK : DCOMW92I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DESCRICAO : CONSULTA INFORMACOES FORMULARIO ADITAMENTO         *
      * DATA      : 25/08/2023                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * TAMANHO   : 1935 BYTES                                         *
      ******************************************************************
       01  AC-ARGUMENTOS-ENTRADA.

       05 DCOMW92I-HEADER.
         10 DCOMW92I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW92I'.
         10 DCOMW92I-TAM-LAYOUT        PIC 9(05)   VALUE 1935.
       05 DCOMW92I-REGISTRO.
         10 DCOMW92I-BLOCO-ENTRADA.
           15 DCOMW92I-E-CONTRATO                  PIC 9(13).
           15 DCOMW92I-E-WORKSPA-DATA              PIC X(20000).
         10 DCOMW92I-BLOCO-SAIDA.
           15 DCOMW92I-S-COD-RET                   PIC 9(03).
           15 DCOMW92I-S-MESSAGEM                  PIC X(100).
