      *----------------------------------------------------------------*
      *         SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR          *
      *         ---------------------------------------------          *
      *   TABELA DE DESCRICOES DOS ERROS ENCONTRADOS NA CONSISTENCIA   *
      *   FISICA DO RETORNO DE SOLICITACOES DE TRAVA/DESTRAVA DE DO-   *
      *   MICILIO BANCARIO ENVIADAS 'A REDECARD                        *
      *   INC: I#BVVEH5                             DATA: 08/08/2008   *
      *----------------------------------------------------------------*
       01  BVVEH5-TB-ERROS-CONSISTENCIA.
           05  FILLER                             PIC X(45) VALUE
               'TIPO DE REGISTRO INVALIDO                    '.
           05  FILLER                             PIC X(45) VALUE
               'BANCO DE ENVIO DIFERENTE DE 237              '.
           05  FILLER                             PIC X(45) VALUE
               'DATA DE PROCESSAMENTO INVALIDA               '.
           05  FILLER                             PIC X(45) VALUE
               'HORARIO DE PROCESSAMENTO INVALIDO            '.
           05  FILLER                             PIC X(45) VALUE
               'VERSAO BRADESCO NAO NUMERICA OU ZERADA       '.
           05  FILLER                             PIC X(45) VALUE
               'VERSAO REDECARD NAO NUMERICA OU ZERADA       '.
           05  FILLER                             PIC X(45) VALUE
               'PONTO DE VENDA NAO NUMERICO OU ZERADO        '.
           05  FILLER                             PIC X(45) VALUE
               'CNPJ/CPF DO ESTAB. NAO NUMERICO OU ZERADO    '.
           05  FILLER                             PIC X(45) VALUE
               'OPERACAO DIFERENTE DE "C" E "D"              '.
           05  FILLER                             PIC X(45) VALUE
               'BANCO DA CONTA CORRENTE DIFERENTE DE 237     '.
           05  FILLER                             PIC X(45) VALUE
               'AGENCIA DA CONTA CORRENTE NAO NUMERICA/ZERADA'.
           05  FILLER                             PIC X(45) VALUE
               'NUMERO DA CONTA CORRENTE NAO NUMERICO/ZERADO '.
           05  FILLER                             PIC X(45) VALUE
               'DIGITO DA CONTA CORRENTE NAO NUMERICO        '.
           05  FILLER                             PIC X(45) VALUE
               'INDICADOR DE TRAVA DIFERENTE DE "T" E "D"    '.
           05  FILLER                             PIC X(45) VALUE
               'TIPO DE PROCESSO DIFERENTE DE "1"            '.
           05  FILLER                             PIC X(45) VALUE
               'SITUACAO DO PROCESSO DIFERENTE DE "P" E "A"  '.
           05  FILLER                             PIC X(45) VALUE
               'DATA DE INICIO DE TRAVA INVALIDA             '.
           05  FILLER                             PIC X(45) VALUE
               'DATA DE TERMINO DE TRAVA INVALIDA            '.
           05  FILLER                             PIC X(45) VALUE
               'DATA DE ENVIO PELO BANCO INVALIDA            '.
           05  FILLER                             PIC X(45) VALUE
               'CNPJ DO ESTAB NA REDECARD INVALIDO           '.
           05  FILLER                             PIC X(45) VALUE
               'DATA FINAL DE TRAVA REDECARD INVALIDA        '.
           05  FILLER                             PIC X(45) VALUE
               'CODIGO DE RETORNO NAO NUMERICO               '.
           05  FILLER                             PIC X(45) VALUE
               'MENSAGEM DE ERRO INVALIDA P/ COD.RETORNO > 0 '.
           05  FILLER                             PIC X(45) VALUE
               'TOTAL DE REGISTROS DO TRAILER NAO BATE       '.
           05  FILLER                             PIC X(45) VALUE
               'TIPO DE DOMICILIO (BANDEIRA) INVALIDO        '.
       01  FILLER  REDEFINES  BVVEH5-TB-ERROS-CONSISTENCIA.
           05  BVVEH5-TB-ERRODESCR    PIC X(45)  OCCURS 25.
