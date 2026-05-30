      *----------------------------------------------------------------*        
      * ELEMENTO  : DBTO - DEBITO AUTOMATICO - INTERFACE 8013          *        
      *----------------------------------------------------------------*        
      * NOME BOOK : DBTOWRE3                                           *        
      * DESCRICAO : RESOLUCAO 74 - BOOK DO ARQUIVO ISD UTILIZADO PARA  *        
      *             GERACAO DE RELATORIO E ARQUIVO PARA O FFPP.        *        
      * EMPRESA   : BRQ SOLUCOES                                       *        
      * AUTOR     : EVANDRO/GUILHERME BEILLO                           *        
      * TAMANHO   : 97 BYTES                                           *        
      *================================================================*        
      *                                                                         
       01 DBTOWRE3-REG.                                                         
          05 WRE3-REG-AREA.                                                     
             10 WRE3-CSERVC-CTRTE                PIC  9(005).                   
             10 WRE3-VSALDO-ANT                  PIC  9(013)V99.                
             10 WRE3-VAMORT                      PIC  9(013)V99.                
             10 WRE3-VLIBER                      PIC  9(013)V99.                
             10 WRE3-VJUROS                      PIC  9(013)V99.                
             10 WRE3-VAJUSTE                     PIC  9(013)V99.                
             10 WRE3-VSALDO-ATUAL                PIC  9(013)V99.                
             10 WRE3-INDCD-GERAC                 PIC  9(001).                   
             10 WRE3-INDCD-TRANSM                PIC  X(001).                   
