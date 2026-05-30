      ******************************************************************
      * NOME BOOK : DCOMW93I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DESCRICAO : CONSULTA INFORMACOES FORMULARIO ADITAMENTO         *
      * DATA      : 25/08/2023                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * TAMANHO   : 1935 BYTES                                         *
      ******************************************************************

       05 DCOMW93I-HEADER.
         10 DCOMW93I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW93I'.
         10 DCOMW93I-TAM-LAYOUT        PIC 9(05)   VALUE 1935.
       05 DCOMW93I-REGISTRO.
         10 DCOMW93I-BLOCO-ENTRADA.
           15 DCOMW93I-E-CONTRATO                  PIC 9(13).
           15 DCOMW93I-E-WORKSPA-DATA              PIC X(20000).
         10 DCOMW93I-BLOCO-SAIDA.
           15 DCOMW93I-S-COD-RET                   PIC 9(03).
           15 DCOMW93I-S-MESSAGEM                  PIC X(100).
