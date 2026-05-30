      ******************************************************************
      * NOME BOOK : DCOMW94I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DESCRICAO : CONSULTA INFORMACOES FORMULARIO ADITAMENTO         *
      * DATA      : 25/08/2023                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * TAMANHO   : 1935 BYTES                                         *
      ******************************************************************

       05 DCOMW94I-HEADER.
         10 DCOMW94I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW94I'.
         10 DCOMW94I-TAM-LAYOUT        PIC 9(05)   VALUE 1935.
       05 DCOMW94I-REGISTRO.
         10 DCOMW94I-BLOCO-ENTRADA.
           15 DCOMW94I-E-CONTRATO                  PIC 9(13).
           15 DCOMW94I-E-WORKSPA-DATA              PIC X(20000).
         10 DCOMW94I-BLOCO-SAIDA.
           15 DCOMW94I-S-COD-RET                   PIC 9(03).
           15 DCOMW94I-S-MESSAGEM                  PIC X(100).
