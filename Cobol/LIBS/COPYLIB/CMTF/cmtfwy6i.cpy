      *================================================================*
      * NOME BOOK  : CMTFWY6I - AREA DE INTERFACE                      *
      * DATA       : SETEMBRO / 2015                                   *
      * AUTOR      : SONDA IT                                          *
      * SISTEMA    : GOTF - GESTAO OPERACIONAL DE TARIFAS              *
      *----------------------------------------------------------------*
      * OBJETIVO   : INCLUIR ADESAO A CESTA DE SERVICO                 *
      *                     - PRE ABERTURA DE CONTAS.                  *
      *----------------------------------------------------------------*
      * TAMANHO    : 00281 BYTES                                       *
      *----------------------------------------------------------------*
      *       +++ D E S C R I C A O  D O S   C A M P O S +++           *
      *----------------------------------------------------------------*
      *                                                                *
      * CMTFWY6I-E-CD-COML-AGENCIA = CODIGO COMERCIAL DA AGENCIA       *
      * CMTFWY6I-E-CD-COML-CONTA   = CODIGO DA CONTA                   *
      * CMTFWY6I-E-CD-CESTA-PERM   = CESTA                             *
      * CMTFWY6I-E-DADOS-CESTA-CEL = DADOS CELULAR                     *
      *                                                                *
      *  -----------------------------------                           *
      *  DADOS REFERENTE AO CLIENTE NEXT                               *
      *  -----------------------------------                           *
      * CMTFWY6I-E-TP-CONTA-NEXT    = TIPO DE CONTA NEXT               *
      *                                   S - SIM                      *
      *                                   N - NAO                      *
      *                                                                *
      * CMTFWY6I-E-CTPO-CANAL     = CODIGO CANAL ENVIADO PELO ELEMENTO *
      * CHAMADOR                                                       *
      *                                                                *
      *  -----------------------------------                           *
      *  DADOS REFERENTE A INFORMACAO PERTENCER OU NAO AO ROLLOUT      *
      *                                                                *
      * CMTFWY6I-E-PERMIS-GOTF    = 'S' SIM ESTA NO ROLLOUT            *
      *                           = 'N' NAO ESTA NO ROLLOUT            *
      *                                                                *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
        05 CMTFWY6I-HEADER.
           10 CMTFWY6I-COD-LAYOUT      PIC  X(008)    VALUE 'CMTFWY6I'.
           10 CMTFWY6I-TAM-LAYOUT      PIC  9(005)    VALUE  281.
      *
        05 CMTFWY6I-BLOCO-RETORNO.
           10 CMTFWY6I-COD-RETORNO     PIC  9(002)    VALUE ZEROS.
           10 CMTFWY6I-COD-MENSAGEM    PIC  9(004)    VALUE ZEROS.
           10 CMTFWY6I-MENSAGEM        PIC  X(079)    VALUE SPACES.
           10 CMTFWY6I-SQLCA           PIC  X(136)    VALUE SPACES.
      *
        05 CMTFWY6I-ENTRADA.
           10  CMTFWY6I-E-AMBIENTE          PIC  X(001)   VALUE SPACES.
           10  CMTFWY6I-E-CD-COML-AGENCIA   PIC  9(005)   VALUE ZEROS.
           10  CMTFWY6I-E-CD-COML-CONTA     PIC  9(007)   VALUE ZEROS.
           10  CMTFWY6I-E-CD-CESTA-PERM     PIC  9(005)   VALUE ZEROS.
           10  CMTFWY6I-E-DADOS-CESTA-CEL.
               15  CMTFWY6I-E-DDD           PIC  X(004) VALUE SPACES.
               15  CMTFWY6I-E-NUMERO        PIC  9(011) VALUE ZEROS.
               15  CMTFWY6I-E-OPERADORA     PIC  9(009) VALUE ZEROS.
           10 CMTFWY6I-E-DADOS-CLIENTE-NEXT.
              15 CMTFWY6I-E-CLIENTE-NEXT    PIC  X(001).
                 88 CMTFWY6I-E-NEXT-SIM          VALUE 'S'.
                 88 CMTFWY6I-E-NEXT-NAO          VALUE 'N'.

           10 CMTFWY6I-E-CTPO-CANAL         PIC  9(003).
           10 CMTFWY6I-E-PERMIS-GOTF        PIC  X(001).
      *
      *================================================================*
      *         ---+   F I M    D O    C O P Y B O O K  +---           *
      *================================================================*
