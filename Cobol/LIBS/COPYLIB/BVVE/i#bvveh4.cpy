      *----------------------------------------------------------------*
      *         SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR          *
      *         ---------------------------------------------          *
      *   TABELA DE DESCRICOES DOS ERROS ENCONTRADOS NA CONSISTENCIA   *
      *   FISICA  DO ARQUIVO  DE  CONFIRMACOES DE DOMICILIO BANCARIO   *
      *   ENVIADO PELA REDECARD                                        *
      *   INC: I#BVVEH4                             DATA: 08/08/2008   *
      *----------------------------------------------------------------*
       01  BVVEH4-TB-ERROS-CONSISTENCIA.
           05  FILLER                             PIC X(45) VALUE
               'TIPO DE REGISTRO INVALIDO                    '.
           05  FILLER                             PIC X(45) VALUE
               'BANCO DE ENVIO DIFERENTE DE 237              '.
           05  FILLER                             PIC X(45) VALUE
               'DATA DE ENVIO INVALIDA                       '.
           05  FILLER                             PIC X(45) VALUE
               'HORARIO DE ENVIO INVALIDO                    '.
           05  FILLER                             PIC X(45) VALUE
               'DATA DE CONTROLE INVALIDA                    '.
           05  FILLER                             PIC X(45) VALUE
               'VERSAO DE ARQUIVO NAO NUMERICA OU ZERADA     '.
           05  FILLER                             PIC X(45) VALUE
               'PONTO DE VENDA NAO NUMERICO OU ZERADO        '.
           05  FILLER                             PIC X(45) VALUE
               'CNPJ/CPF DO ESTAB. NAO NUMERICO OU ZERADO    '.
           05  FILLER                             PIC X(45) VALUE
               'FILIAL DO CNPJ ZERADA COM TIPO PESSOA = "J"  '.
           05  FILLER                             PIC X(45) VALUE
               'TIPO DE PESSOA DIFERENTE DE "F" E "J"        '.
           05  FILLER                             PIC X(45) VALUE
               'AGENCIA DA CONTA CORRENTE NAO NUMERICA/ZERADA'.
           05  FILLER                             PIC X(45) VALUE
               'NUMERO DA CONTA CORRENTE NAO NUMERICO/ZERADO '.
           05  FILLER                             PIC X(45) VALUE
               'DIGITO DA CONTA CORRENTE INVALIDO            '.
           05  FILLER                             PIC X(45) VALUE
               'NUMERO SEQUENCIAL NAO NUMERICO OU ZERADO     '.
           05  FILLER                             PIC X(45) VALUE
               'INDICATIVO BASE DIFERENTE DE "L" E "E"       '.
           05  FILLER                             PIC X(45) VALUE
               'TIPO DE ALTERACAO DIFERENTE DE "1"           '.
           05  FILLER                             PIC X(45) VALUE
               'DATA DE SOLICITACAO DA ALTERACAO INVALIDA    '.
           05  FILLER                             PIC X(45) VALUE
               'TIPO DE PROCESSO DIFERENTE DE "1"            '.
           05  FILLER                             PIC X(45) VALUE
               'TOTAL DE REGISTROS DO TRAILER NAO BATE       '.
       01  FILLER  REDEFINES  BVVEH4-TB-ERROS-CONSISTENCIA.
           05  BVVEH4-TB-ERRODESCR    PIC X(45)  OCCURS 19.
