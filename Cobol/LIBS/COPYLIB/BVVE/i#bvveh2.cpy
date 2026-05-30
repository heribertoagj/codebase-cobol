      *----------------------------------------------------------------*
      *         SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR          *
      *         ---------------------------------------------          *
      *   TABELA DE DESCRICOES DOS ERROS ENCONTRADOS NA CONSISTENCIA   *
      *   FISICA DO RETORNO DE SOLICITACOES DE AFILIACOES DE ESTABE-   *
      *   LECIMENTOS ENVIADAS 'A REDECARD                              *
      *   INC: I#BVVEH2                             DATA: 08/08/2008   *
      *----------------------------------------------------------------*
       01  BVVEH2-TB-ERROS-CONSISTENCIA.
           05  FILLER                             PIC X(44) VALUE
               'TIPO DE REGISTRO INVALIDO                   '.
           05  FILLER                             PIC X(44) VALUE
               'LOTE NAO NUMERICO OU ZERADO                 '.
           05  FILLER                             PIC X(44) VALUE
               'DATA DO LOTE INVALIDA                       '.
           05  FILLER                             PIC X(44) VALUE
               'HORARIO DO LOTE INVALIDO                    '.
           05  FILLER                             PIC X(44) VALUE
               'EMISSOR DIFERENTE DE 237                    '.
           05  FILLER                             PIC X(44) VALUE
               'NUMERO DE REGISTRO NAO NUMERICO             '.
           05  FILLER                             PIC X(44) VALUE
               'TIPO DE PESSOA DIFERENTE DE "F" E "J"       '.
           05  FILLER                             PIC X(44) VALUE
               'CNPJ/CPF DO ESTAB. NAO NUMERICO OU ZERADO   '.
           05  FILLER                             PIC X(44) VALUE
               'FILIAL DO CNPJ ZERADA COM TIPO PESSOA = "J" '.
           05  FILLER                             PIC X(44) VALUE
               'INDICADOR DE FILIACAO NAO ESTA ENTRE A-Z    '.
           05  FILLER                             PIC X(44) VALUE
               'CODIGO DE MOTIVO INVALIDO                   '.
           05  FILLER                             PIC X(44) VALUE
               'DATA DE FILIACAO INVALIDA                   '.
           05  FILLER                             PIC X(44) VALUE
               'DESCRICAO MOTIVO INVALIDA P/ COD.MOTIVO > 0 '.
           05  FILLER                             PIC X(44) VALUE
               'NUMERO DE PONTO DE VENDA INVALIDO           '.
           05  FILLER                             PIC X(44) VALUE
               'RAZAO SOCIAL INVALIDA                       '.
           05  FILLER                             PIC X(44) VALUE
               'TIPO DE MOVIMENTO DIFERENTE DE "E" E "S"    '.
           05  FILLER                             PIC X(44) VALUE
               'NUMERO HE NAO NUMERICO                      '.
           05  FILLER                             PIC X(44) VALUE
               'DATA DE ABERTURA DA SOLICITACAO INVALIDA    '.
           05  FILLER                             PIC X(44) VALUE
               'DATA PREVISAO INSTAL.MAQUINETA INVALIDA     '.
           05  FILLER                             PIC X(44) VALUE
               'DATA INSTAL.MAQUINETA INVALIDA              '.
           05  FILLER                             PIC X(44) VALUE
               'DATA DE PREVISAO INSTAL.TECNOLOGIA INVALIDA '.
           05  FILLER                             PIC X(44) VALUE
               'DATA INSTAL.TECNOLOGIA INVALIDA             '.
           05  FILLER                             PIC X(44) VALUE
               'INDIC.INTERNET INVALIDO                     '.
           05  FILLER                             PIC X(44) VALUE
               'CODIGO DE CANAL INVALIDO                    '.
           05  FILLER                             PIC X(44) VALUE
               'AGENCIA DE FILIACAO INVALIDA                '.
           05  FILLER                             PIC X(44) VALUE
               'CNPJ/CPF DO VENDEDOR INVALIDO               '.
           05  FILLER                             PIC X(44) VALUE
               'FILIAL DO CNPJ DO VENDEDOR INVALIDA         '.
           05  FILLER                             PIC X(44) VALUE
               'CODIGO DE CELULA INVALIDO                   '.
           05  FILLER                             PIC X(44) VALUE
               'TOTAL DE REGISTROS DO TRAILER NAO BATE      '.
       01  FILLER  REDEFINES  BVVEH2-TB-ERROS-CONSISTENCIA.
           05  BVVEH2-TB-ERRODESCR    PIC X(44)  OCCURS 29.
