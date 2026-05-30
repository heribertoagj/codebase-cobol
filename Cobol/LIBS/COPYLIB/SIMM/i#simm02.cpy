      *----------------------------------------------------------------*
      *    INC 'I#SIMM02'       - AREA DE COMUNICACAO DE MENSAGENS     *
      *    TAMANHO              - 0269 BYTES                           *
      *                                                                *
      *    ANALISTA RESPONSAVEL - MARCILENE/INES(PRIME)                *
      *    DESCRICAO            - BLOCO FINANCEIRO  BRADESCO           *
      *                                                                *
      *    ATUALIZACAO          - 31/10/2002                           *
      *    DESCRICAO            - INCLUSAO DO CAMPO QUE INFORMA ERRO   *
      *                           DA DESCRIPTOGRAFIA - ERRO-GEN0004    *
      *----------------------------------------------------------------*
      *    ATUALIZACAO          - 07/04/2003                           *
      *    DESCRICAO            - INCLUSAO DO CAMPO IDENT-GER          *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ATUALIZACAO          - 14/02/2005                           *
      *    DESCRICAO            - INCLUSAO DO CAMPO IDENT-REENVIO      *
      *----------------------------------------------------------------*
      *    ATUALIZACAO          - 10/02/2009                           *
      *    DESCRICAO            - INCLUSAO DO CAMPO INTER-AGENCIA      *
      *----------------------------------------------------------------*
PRI755*    ATUALIZACAO          - 20/03/2013                           *
.     *    RESPONSAVEL          - JORGE KAUE - PRIME                   *
PRI755*    DESCRICAO            - INCLUSAO DO CAMPO INDCD-PGTO         *
      *----------------------------------------------------------------*
PRIME *    ATUALIZACAO          - 02/04/2014                           *
PRIME *    RESPONSAVEL          - PRIME INFORMATICA                    *
PRIME *    DESCRICAO            - INCLUIDO MOVIMENTACAO DE VALORES DE  *
PRIME *                           80 A 94 NO CAMPO TIPO DE MIDIA PARA  *
PRIME *                           ATENDER AO PROJETO TESTE DE CARGA.   *
      *----------------------------------------------------------------*
      *
       01  SIMM02-REGISTRO.
           03  SIMM02-BLOCO-FINANC-BDSCO.
               05  SIMM02-IBLOCO            PIC  X(0008).
               05  SIMM02-EVENTO.
                   07  SIMM02-CSIST-EXTER   PIC  X(0003).
                   07  SIMM02-CEVNTO-MSGEM  PIC  X(0004).
                   07  SIMM02-CFLUXO-MSGEM  PIC  X(0002).
               05  SIMM02-CNRO-CTRL-INSTC   PIC  X(0020).
               05  SIMM02-SIT-LCTO          PIC  X(0004).
               05  SIMM02-CFNALD-LCTO       PIC  X(0014).
               05  SIMM02-GVLR-LCTO.
                   07  SIMM02-CVLR-LCTO     PIC  9(0002).
                   07  SIMM02-VLR-LCTO      PIC  9(0015)V99.
               05  SIMM02-GVLR-DEP.
                   07  SIMM02-CVLR-DEP      PIC  9(0002).
                   07  SIMM02-VLR-DEP       PIC  9(0015)V99.
               05  SIMM02-CIDTFD-SNAL-MOVTO PIC  X(0001).
               05  SIMM02-GDMOVTO.
                   07  SIMM02-DMOVTO        PIC  9(0008).
               05  SIMM02-HRECEP.
                   07  SIMM02-HHRECEP       PIC  9(0002).
                   07  SIMM02-MMRECEP       PIC  9(0002).
                   07  SIMM02-SSRECEP       PIC  9(0002).
               05  SIMM02-CTRL-ORIG         PIC  X(0020).
               05  SIMM02-CTRL-LIQUID       PIC  X(0020).
               05  SIMM02-CTRL-TARIFA       PIC  X(0020).
               05  SIMM02-GTARIFA.
                   07  SIMM02-CTARIFA-BB    PIC  9(0002).
                   07  SIMM02-TARIFA-BB     PIC  9(0015)V99.
               05  SIMM02-GCMOVTO.
                   07  SIMM02-CMOVTO        PIC  9(0005).
               05  SIMM02-GDEPDC.
                   07  SIMM02-CDEPDC-MOVTO  PIC  9(0005).
               05  SIMM02-GDAGEND.
                   07  SIMM02-DAGEND-MOVTO  PIC  9(0008).
               05  SIMM02-GHAGEND.
                   07  SIMM02-HAGEND-MOVTO  PIC  9(0006).
               05  SIMM02-LLIQDC-MOVTO      PIC  X(0003).
               05  SIMM02-IND-ACAO          PIC  X(0001).
               05  SIMM02-CBOLETO           PIC  X(0015).
               05  SIMM02-TP-HPREV          PIC  X(0001).
               05  SIMM02-GDTHREQ.
                   07  SIMM02-DT-HREQ       PIC  9(0014).
               05  SIMM02-GTPMIDIA.
                   07  SIMM02-TP-MIDIA      PIC  9(0002).
               05  SIMM02-CCUSTO-ORIGEM     PIC  X(0004).
               05  SIMM02-GSTATUS.
                   07  SIMM02-STATUS-LEGI   PIC  9(0002).
               05  SIMM02-ERRO-GEN0004      PIC  9(0004).
               05  SIMM02-IDENT-GER         PIC  X(0001).
      *               A - EMPURRA DE MENSAGENS
      *               B - EGEN0050
      *               C - CORRECAO DE MENSAGENS COM FLUXO 'E'
               05  SIMM02-TAM-ARQG15        PIC  9(0007).
               05  SIMM02-IDENT-REENVIO     PIC  X(0001).
               05  SIMM02-INTER-AGENCIA     PIC  X(0001).
PRI755         05  SIMM02-INDCD-PGTO        PIC  X(0001).
PRI622         05  SIMM02-REGRA-TESTE       PIC  X(0001).
