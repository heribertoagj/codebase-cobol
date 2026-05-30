      *----------------------------------------------------------------*
      *         SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR          *
      *         ---------------------------------------------          *
      *   TABELA DE DESCRICOES DOS ERROS ENCONTRADOS NA CONSISTENCIA   *
      *   FISICA DO RETORNO DE  SOLICITACOES DE TRANSFERENCIA DE DO-   *
      *   MICILIO BANCARIO ENVIADAS 'A REDECARD                        *
      *   INC: I#BVVEH3                             DATA: 08/08/2008   *
      *----------------------------------------------------------------*
       01  BVVEH3-TB-ERROS-CONSISTENCIA.
           05  FILLER                             PIC X(45) VALUE
               'TIPO DE REGISTRO INVALIDO                    '.
           05  FILLER                             PIC X(45) VALUE
               'BANCO DE ENVIO DIFERENTE DE 237              '.
           05  FILLER                             PIC X(45) VALUE
               'DATA DE ENVIO INVALIDA                       '.
           05  FILLER                             PIC X(45) VALUE
               'HORARIO DE ENVIO INVALIDO                    '.
           05  FILLER                             PIC X(45) VALUE
               'VERSAO DE ARQUIVO NAO NUMERICA OU ZERADA     '.
           05  FILLER                             PIC X(45) VALUE
               'RAZAO SOCIAL INVALIDA                        '.
           05  FILLER                             PIC X(45) VALUE
               'PONTO DE VENDA NAO NUMERICO OU ZERADO        '.
           05  FILLER                             PIC X(45) VALUE
               'CNPJ/CPF DO ESTAB. NAO NUMERICO OU ZERADO    '.
           05  FILLER                             PIC X(45) VALUE
               'FILIAL DO CNPJ ZERADA COM TIPO PESSOA = "J"  '.
           05  FILLER                             PIC X(45) VALUE
               'TIPO DE PESSOA DIFERENTE DE "F" E "J"        '.
           05  FILLER                             PIC X(45) VALUE
               'OPERACAO DIFERENTE DE "C" E "D"              '.
           05  FILLER                             PIC X(45) VALUE
               'BANCO DA CONTA CORRENTE DIFERENTE DE 237     '.
           05  FILLER                             PIC X(45) VALUE
               'AGENCIA DA CONTA CORRENTE NAO NUMERICA/ZERADA'.
           05  FILLER                             PIC X(45) VALUE
               'NUMERO DA CONTA CORRENTE NAO NUMERICO/ZERADO '.
           05  FILLER                             PIC X(45) VALUE
               'DIGITO DA CONTA CORRENTE INVALIDO            '.
           05  FILLER                             PIC X(45) VALUE
               'INDICADOR DE TRAVA DIFERENTE DE "T" E "N"    '.
           05  FILLER                             PIC X(45) VALUE
               'TIPO DE PROCESSO DIFERENTE DE "1"            '.
           05  FILLER                             PIC X(45) VALUE
               'DATA DE ENVIO PELO BANCO INVALIDA            '.
           05  FILLER                             PIC X(45) VALUE
               'CODIGO DE RETORNO NAO NUMERICO               '.
           05  FILLER                             PIC X(45) VALUE
               'MENSAGEM DE ERRO INVALIDA P/ COD.RETORNO > 0 '.
           05  FILLER                             PIC X(45) VALUE
               'PONTO DE VENDA CENTRALIZADOR NAO NUMERICO    '.
           05  FILLER                             PIC X(45) VALUE
               'SIGLA PRODUTO DIFERENTE DE CR,DB,VC,VD E DC  '.
           05  FILLER                             PIC X(45) VALUE
               'TOTAL DE REGISTROS DO TRAILER NAO BATE       '.
       01  FILLER  REDEFINES  BVVEH3-TB-ERROS-CONSISTENCIA.
           05  BVVEH3-TB-ERRODESCR    PIC X(45)  OCCURS 23.
