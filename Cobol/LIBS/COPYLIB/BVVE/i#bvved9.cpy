      *----------------------------------------------------------------*
      *  I#BVVED9 - ARQUIVO DE ANTECIPACOES DO CPF/CNPJ *
      *  DSNAME: CB.BACC.VON.TRAVPANT                                  *
      *----------------------------------------------------------------*

       01  BVVED9-MENSAGENS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0001'.
           03 FILLER                   PIC X(100)  VALUE
              'REJEITADO POR GARANTIA (*) JA REGISTRADA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0003'.
           03 FILLER                   PIC X(100)  VALUE
              'TIPO DE PESSOA FORA DO DOMINIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0004'.
           03 FILLER                   PIC X(100)  VALUE
               'TIPO DE PRODUTO FORA DO DOMINIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0007'.
           03 FILLER                   PIC X(100)  VALUE
              'CPF OU CNPJ INVALIDO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0008'.
           03 FILLER                   PIC X(100)  VALUE
              'CODIGO DO PROPRIETARIO DO ESQUEMA FORA DO DOMINIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0010'.
           03 FILLER                   PIC X(100)  VALUE
              'AGENCIA INVALIDA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0011'.
           03 FILLER                   PIC X(100)  VALUE
               'CONTA INVALIDA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0012'.
           03 FILLER                   PIC X(100)  VALUE
              'DOMICILIO BANCARIO JA BAIXADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0014'.
           03 FILLER                   PIC X(100)  VALUE
              'PERCENTUAL DE GARANTIA DIFERENTE 100'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0015'.
           03 FILLER                   PIC X(100)  VALUE
               'DATA INICIO DE VALIDADE MAIOR OU IGUAL A DATA DE REFEREN
      -        'CIA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0016'.
           03 FILLER                   PIC X(100)  VALUE
               'DATA FIM DE VALIDADE MAIOR QUE O PERIODO MAXIMO DE VALID
      -        'ADE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0017'.
           03 FILLER                   PIC X(100)  VALUE
               'DATA FIM DE VALIDADE MENOR QUE A DATA DE REFERENCIA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0018'.
           03 FILLER                   PIC X(100)  VALUE
               'DATA INICIO DE VALIDADE MAIOR QUE DATA FIM DE VALIDADE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0019'.
           03 FILLER                   PIC X(100)  VALUE
              'ALTERACAO NAO PERMITIDA PARA DOMICILIO BANCARIO BAIXADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0020'.
           03 FILLER                   PIC X(100)  VALUE
              'DOMICILIO BANCARIO ATIVO PARA OUTRO PARTICIPANTE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0027'.
           03 FILLER                   PIC X(100)  VALUE
              'TIPO DE PRODUTO DIFERENTE DE AMBOS - DATA DE INICIO DE VA
      -       'LIDADE MAIOR OU IGUAL A 19.07.2010'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0029'.
           03 FILLER                   PIC X(100)  VALUE
              'REQUISICAO ENVIADA FORA DA GRADE HORARIA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0031'.
           03 FILLER                   PIC X(100)  VALUE
              'CNPJ_CPF CLIENTE DIFERENTE DO CNPJ BASE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0032'.
           03 FILLER                   PIC X(100)  VALUE
              'A DATA INFORMADA E DIFERENTE DA DATA DE REFERENCIA DO SIS
      -       'TEMA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0037'.
           03 FILLER                   PIC X(100)  VALUE
              'NUMERO DO CODIGO DA IF INVALIDO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0042'.
           03 FILLER                   PIC X(100)  VALUE
              'NAO EXISTE ADESAO PARA ESSA FUNCIONALIDADE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0046'.
           03 FILLER                   PIC X(100)  VALUE
              'CONSULTA NAO ENCONTRADA'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0054'.
           03 FILLER                   PIC X(100)  VALUE
              'REJEITADO POR GARANTIA (*) JA REGISTRADA POR CNPJ COMPLET
      -       'O'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0055'.
           03 FILLER                   PIC X(100)  VALUE
               'ESTABELECIMENTO SEM CREDENCIAMENTO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0056'.
           03 FILLER                   PIC X(100)  VALUE
              'NUMERO DO CODIGO DA IF DIFERENTE DA IF SOLICITANTE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0057'.
           03 FILLER                   PIC X(100)  VALUE
              'NUMERO DE IDENTIFICADOR SCG NAO CADASTRADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0058'.
           03 FILLER                   PIC X(100)  VALUE
              'INCLUSAO NAO PERMITIDA POR CNPJ COMPLETO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0059'.
           03 FILLER                   PIC X(100)  VALUE
              'INDICADOR DE ANTECIPACAO FORA DO DOMINIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0060'.
           03 FILLER                   PIC X(100)  VALUE
              'TIPO DE RETORNO FORA DO DOMINIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0064'.
           03 FILLER                   PIC X(100)  VALUE
              'TIPO DE BAIXA NAO PERMITIDA - UTILIZADA PELO SCG'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0069'.
           03 FILLER                   PIC X(100)  VALUE
              'ISPB DESTINATARIO INVALIDO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0070'.
           03 FILLER                   PIC X(100)  VALUE
              'NOME DO ARQUIVO INVALIDO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0071'.
           03 FILLER                   PIC X(100)  VALUE
              'TIPO DE TRANSMISSAO NAO UTILIZADO NO SCG'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0072'.
           03 FILLER                   PIC X(100)  VALUE
              'TIPO DE TRANSMISSAO FORA DO DOMINIO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0074'.
           03 FILLER                   PIC X(100)  VALUE
              'INCLUSAO NAO PERMITIDA POR CNPJ BASE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0079'.
           03 FILLER                   PIC X(100)  VALUE
              'A TAG ISPBIF NAO PODE SER DIFERENTE DA TAG IDENTDEMISSOR
      -       ' '.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0080'.
           03 FILLER                   PIC X(100)  VALUE
              'INCLUSAO NAO PERMITIDA: EXISTEM REGISTROS DE DOMICILIOS B
      -       'ANCARIOS DE DEBITO E CREDITO ATIVOS'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0081'.
           03 FILLER                   PIC X(100)  VALUE
              'INCLUSAO NAO PERMITIDA: EXISTE REGISTRO DE DOMICILIO BANC
      -       'ARIO DE DEBITO ATIVO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0082'.
           03 FILLER                   PIC X(100)  VALUE
              'INCLUSAO NAO PERMITIDA: EXISTE REGISTRO DE DOMICILIO BANC
      -       'ARIO DE CREDITO ATIVO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0083'.
           03 FILLER                   PIC X(100)  VALUE
              'INCLUSAO NAO PERMITIDA: EXISTEM REGISTROS DE DOM. BANCARI
      -       'OS DE DEB/CRED ATIVOS EM IFS DIFERENTES'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0084'.
           03 FILLER                   PIC X(100)  VALUE
              'ALTERACAO NAO PERMITIDA: EXISTEM REGISTROS DE DOMICILIOS
      -       'BANCARIOS DE DEBITO OU CREDITO ATIVOS'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0085'.
           03 FILLER                   PIC X(100)  VALUE
              'DATA INICIO DE VALIDADE MENOR QUE DATA LIMITE DE RETROATI
      -       'VIDADE'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'ESCG0999'.
           03 FILLER                   PIC X(100)  VALUE
              'ERRO INESPERADO'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(008)  VALUE
              'PROTMNF '.
           03 FILLER                   PIC X(100)  VALUE
              'MANUTENCAO SOLICITADA PELO MASSIVO NAO OBTEVE RETORNO DA
      -       'CIP'.
           03 FILLER                   PIC 9(009)  COMP-3 VALUE ZEROS.
           03 FILLER                   PIC X(6215).
       01  BVVED9-MENSAGENS-R          REDEFINES BVVED9-MENSAGENS.
           03 FILLER                   OCCURS 99.
               05 BVVED9-COD-MSG       PIC X(008).
               05 BVVED9-TXT-MSG       PIC X(100).
               05 BVVED9-ACU-MSG       PIC 9(009) COMP-3.
