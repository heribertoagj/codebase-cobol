      *---------------------------------------------------------------*
      *              S O N D A      P R O C W O R K                   *
      *---------------------------------------------------------------*
      * INCLUDE.....: I#RUEC65 - PARA MODULO RUEC8810                 *
      * GERACAO.....: AGOSTO/2012                                     *
      *---------------------------------------------------------------*
      * OBJETIVO....: AREA DE COMUNICACAO PARA FORMATACAO/REFORMATACAO*
      *               PARA ENVIO DOS DADOS PARA RECOR                 *
      *---------------------------------------------------------------*
      * PROJETO ....: REFORMULACAO MENSAGERIA BACEN - RECOR           *
      *---------------------------------------------------------------*
NAV506*                    ALTERACAO - NAVA                            *
NAV506*----------------------------------------------------------------*
NAV506*  ANALISTA ......:  NAVA                                        *
NAV506*  DATA...........:  MAI/2023                                    *
NAV506*----------------------------------------------------------------*
NAV506*  OBJETIVO......:   ADEQUAR PARA SICOR 5.06                     *
NAV506*----------------------------------------------------------------*
      **
       01  RUEC65-AREA.
           05 RUEC65-MSG.
              10 RUEC65-PGM-CHAMADOR         PIC  X(008).
              10 RUEC65-CCONTR               PIC  9(009).
              10 RUEC65-NENVIO-RECOR         PIC  9(005).
              10 RUEC65-FUNCAO               PIC  X(001).
NAV506        10 RUEC65-TP-DESCLASS          PIC  X(001).
NAV506        10 FILLER                      PIC  X(029).
NAV506***     10 FILLER                      PIC  X(030).
     **
     **        'F' - FORMATA SISMSG E ENVIA RECOR
     **        'R' - REENVIA SISMSG JA FORMATADO PARA RECOR
     **
NAV506**        RUEC65-TP-DESCLASS
NAV506**        'T' - DESCLASSIFICACAO TOTAL
NAV506**        'P' - DESCLASSIFICACAO PARCIAL
NAV506**
     *************** AREA DE DADOS DE RETORNO ***************
     **
           05 RUEC65-RETORNO.
              10 RUEC65-COD-RET              PIC  9(002).
     **
     **=======  00 - OK
     **=======  01 - DADOS INCONSISTENTES
     **=======  03 - CONTRATO NAO ENCONTRADO
     **=======  99 - ERRO DB2
              10 RUEC65-MENSAGEM             PIC  X(079).
              10 FILLER                      PIC  X(050).
              10 RUEC65-SQLCA                PIC  X(136).
              10 RUEC65-ERRO-AREA            PIC  X(107).
