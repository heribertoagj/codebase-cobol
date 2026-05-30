       01  ENTRADA-6435-ROTEADOR.
           03  ROTENT-6435-COD-RETORNO                PIC  X(04).
           03  ROTENT-6435-MSG-RETORNO                PIC  X(79).
           03  ROTENT-6435-RESTART                    PIC  9(05).
           03  ROTENT-6435-FLAG                       PIC  X(01).
           03  ROTENT-6435-START-COUNT                PIC  X(01).
           03  ROTENT-6435-CFUNC-BDSCO                PIC  9(09).
           03  ROTENT-6435-CTERM                      PIC  X(08).
           03  ROTENT-6435-CPRODT                     PIC  9(03).
           03  ROTENT-6435-CTPO-PESQUISA              PIC  X(01).
      *        O=OPERACAO,  C=CONTRATO LIMITE  V=CONVENIO
           03  ROTENT-6435-MEIO-ENTRADA               PIC  9(03).
      *        INFORMAR MEIO DE ENTRADA SOH PARA OPERACAO
           03  ROTENT-6435-COD-GARANTIA               PIC  9(03).
      *        INFORMAR CODIGO DA GARANTIA INDIVIDUAL A PESQUISAR
           03  ROTENT-6435-CSPROD-DESC-COML-X  OCCURS  15 TIMES.
               05 ROTENT-6435-CSPROD-DESC-COML        PIC  9(03).

       01  SAIDA-6435-ROTEADOR.
           03  ROTSAI-6435-COD-RETORNO                PIC  X(04).
           03  ROTSAI-6435-MSG-RETORNO                PIC  X(79).
           03  ROTSAI-6435-RESTART                    PIC  9(05).
           03  ROTSAI-6435-FLAG                       PIC  X(01).
           03  ROTSAI-6435-QTDE-RETORNADA             PIC  9(03).
           03  ROTSAI-6435-COUNT                      PIC  9(09).
           03  ROTSAI-6435-DADOS.
               05  ROTSAI-6435-TABELA-SAIDA           OCCURS 10  TIMES.
                   07  ROTSAI-6435-COD-GARANTIA      PIC  9(03).
                   07  ROTSAI-6435-DESC-GARANTIA     PIC  X(40).
