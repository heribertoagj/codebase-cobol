       01  ENTRADA-6401-ROTEADOR.
           03  ROTENT-6401-COD-RETORNO        PIC  X(04).
           03  ROTENT-6401-MSG-RETORNO        PIC  X(79).
           03  ROTENT-6401-RESTART            PIC  9(05).
           03  ROTENT-6401-FLAG               PIC  X(01).
           03  ROTENT-6401-START-COUNT        PIC  X(01).
           03  ROTENT-6401-CFUNC-BDSCO        PIC  9(09).
           03  ROTENT-6401-CTERM              PIC  X(08).
           03  ROTENT-6401-CPROD              PIC  9(03).
           03  ROTENT-6401-CMEIO-ENTRD-DESC   PIC  9(03).
           03  ROTENT-6401-CCNPJ-CPF          PIC  9(09).
           03  ROTENT-6401-CFLIAL-CNPJ        PIC  9(05).
           03  ROTENT-6401-CCTRL-CNPJ-CPF     PIC  9(02).
           03  ROTENT-6401-CCART-NORML-DESC   PIC  X(05).
           03  ROTENT-6401-CTPO-PESQUISA      PIC  X(01).
*******
*******   PARAMETROS DE ENTRADA P/ ROTENT-6401-CTPO-PESQUISA
*******
*******   O=OPERACAO, C=CONTRATO LIMITE, V=CONVENIO.
*******   X=CONSULTA CONTRATO LIMITE, Y=CONSULTA CONVENIO.
*******   X E Y NAO TRATA RETORNO DO CONE, MOVE 0 PARA
*******   ROTSAI-6401-COD-RETORNO QUANDO CONE0456-RETURN-CODE
*******   DIFERENTE DE 0 E 99.
*******   WEB - TRATAVA RETORNO 908 NA CONSULTA POR C E V.
*******
           03  ROTENT-6401-CSPROD-DESC-X OCCURS 10 TIMES.
               05 ROTENT-6401-CSPROD-DESC-COML PIC 9(03).
           03  ROTENT-6401-CAG-BCRIA           PIC 9(05).

       01  SAIDA-6401-ROTEADOR.
           03  ROTSAI-6401-HEADER.
               05  ROTSAI-6401-COD-RETORNO            PIC  X(04).
               05  ROTSAI-6401-MSG-RETORNO            PIC  X(79).
               05  ROTSAI-6401-RESTART                PIC  9(05).
               05  ROTSAI-6401-FLAG                   PIC  X(01).
               05  ROTSAI-6401-QTDE-RETORNADA         PIC  9(03).
               05  ROTSAI-6401-COUNT                  PIC  9(09).
               05  ROTSAI-6401-VLR-CONTR-LIM          PIC  9(15)V99.
      *            VALOR SUMARIZADO DISPONIVEL PARA CONTRATO LIMITE
           03  ROTSAI-6401-DADOS.
               05  ROTSAI-6401-TABELA-SAIDA  OCCURS 10 TIMES.
                   07  ROTSAI-6401-CCART              PIC  X(05).
                   07  ROTSAI-6401-NRO-CONS-NEGOC     PIC  9(18).
                   07  ROTSAI-6401-VLR-PRINC-LMTE     PIC  9(15)V99.
                   07  ROTSAI-6401-SLD-DISPO-LMTE     PIC  9(15)V99.
                   07  ROTSAI-6401-DVCTO-LMTE         PIC  X(10).
                   07  ROTSAI-6401-CGARNT             PIC  9(03).
                   07  ROTSAI-6401-IGARNT             PIC  X(30).
                   07  ROTSAI-6401-CSIT-CONS-NEGOC    PIC  9(03).
                   07  ROTSAI-6401-ISIT-CONS-NEGOC    PIC  X(30).
DTS001             07  ROTSAI-6401-SLD-PLUS           PIC  9(15)V99.
           03  ROTSAI-6401-CONSISTENCIA.
               05  ROTSAI-6401-TAMANHO-ERRO           PIC  9(01).
               05  ROTSAI-6401-TABELA-ERROS   OCCURS 08  TIMES.
                   07  ROTSAI-6401-CAMPO-ERRO         PIC  9(01).

